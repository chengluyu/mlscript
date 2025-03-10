package hkmc2
package semantics
package ucs
package rp

import mlscript.utils.*, shorthands.*
import Message.MessageContext, syntax.Literal, syntax.Tree.Ident, Pattern.*
import collection.immutable.VectorBuilder
import collection.mutable.{Buffer, Map as MutMap, Queue as MutQueue}

class Compiler(elaborator: Elaborator)(using Raise):
  import Compiler.*, elaborator.tl.*
  
  given Elaborator.State = elaborator.state
  
  extension (pattern: Pattern)
    /** Instantiate a pattern with a map of pattern parameters */
    def instantiate(using parameterMap: Map[VarSymbol, Pattern]): Pattern = trace(
      pre = s"instantiate <<< ${pattern.display} <<< " + parameterMap.iterator.map:
        case (symbol, pattern) => s"${symbol.name} => ${pattern.display}"
      .mkString("{", ", ", "}"),
      post = (res: Pattern) => s"instantiate >>> ${res.display}",
      scope = "ucs:npc:inst"
    ):
      pattern match
        case Constructor(symbol, subPatterns) =>
          Constructor(symbol, subPatterns.map(_.instantiate))
        case NonTerminal(symbol: PatternSymbol, patternParameters, subPatterns) =>
          require(symbol.patternParameterCount == patternParameters.size)
          NonTerminal(symbol, patternParameters.map(_.instantiate), subPatterns.map(_.instantiate))
        case NonTerminal(symbol: VarSymbol, patternParameters, subPatterns) =>
          require(patternParameters.isEmpty)
          parameterMap.get(symbol) match
            case S(pattern) =>
              if subPatterns.nonEmpty then
                // TODO: Add locations during the elaboration stage.
                error(msg"Unable to obtain extracts from pattern parameters" -> None)
              pattern
            case N =>
              // It should have been rejected during the elaboration stage.
              lastWords(s"Pattern name not found: ${symbol.name}")
        case Alternative(first, second) => Alternative(first.instantiate, second.instantiate)
        case Wildcard | Constant(_) => pattern
        case pattern @ Transformed(innerPattern, extracts) =>
          Transformed(innerPattern.instantiate, extracts)(pattern.parameters, pattern.transformer)
  
  /** Enumerate all situations that can be represented by the input patterns.
   *  Each situation is associated with the index of the input pattern. */
  private def enumerateSituations(patterns: Vector[Pattern]): Vector[Situation] =
    def expand(pattern: Pattern)(using index: Int): Vector[Situation] = trace(
      pre = s"expand <<< ${pattern.display} <<< $index",
      post = (res: Vector[Situation]) => s"expand >>>",
      scope = "ucs:npc:expand"
    ):
      pattern match
        case pattern: Constructor => Vector(Situation(pattern))
        case NonTerminal(symbol: PatternSymbol, patternParameters, subPatterns) =>
          require(symbol.patternParameterCount == patternParameters.size)
          val instantiatedPattern = if patternParameters.isEmpty then
            symbol.pattern
          else
            val parameterMap = symbol.patternParams.iterator.map(_.sym).zip(patternParameters).toMap
            symbol.pattern.instantiate(using parameterMap)
          expand(instantiatedPattern)
        case NonTerminal(symbol: VarSymbol, patternParameters, subPatterns) =>
          require(patternParameters.isEmpty)
          lastWords(s"Found a pattern parameter that has not yet been instantiated: ${symbol.name}")
        case Alternative(first, second) => expand(first) ++ expand(second)
        case pattern: Constant => Vector(Situation(pattern))
        case pattern: Wildcard.type => Vector(Situation(pattern))
        case pattern @ Transformed(innerPattern, extracts) => innerPattern match
          case innerPattern: SimplePattern =>
            Vector(new Situation(innerPattern, extracts, pattern.parameters, pattern.transformer, index))
          case _ =>
            // I need to find out the way to compose nested `Transformed`
            // patterns and the way to extracts from both sides of `Alternative`.
            TODO(s"This is not a simple pattern: ${pattern.display}")
        
    patterns.iterator.zipWithIndex.flatMap(expand(_)(using _)).toVector
  
  /** This method removes duplicate patterns from the same column; at the same
   *  time, it unifies identical patterns by merging their extracts and
   *  maintains a mapping from each pattern's sub-extracts to the corresponding
   *  unified pattern's sub-extracts. */
  private def reconcilePatternSubExtracts(patterns: Vector[(Pattern, Vector[(Path, Int)])]) =
    val seenPatterns = MutMap.empty[Pattern, (uniqueIndex: Int, extracts: Vector[Path])]
    val unique = VectorBuilder[Pattern]()
    val mapping = VectorBuilder[(uniqueIndex: Int, extractMapping: Vector[Int])]()
    //                    from original to unified ^^^^^^^^^^^^^^
    
    patterns.foreach:
      case (pattern, extractIndexPairs) => seenPatterns.updateWith(pattern):
        case Some((uniqueIndex, extracts)) => // TODO: Scala's warning.
          // The unified extract vector might include more elements than each
          // original extract vector (a proper superset), so we create a mapping
          // to indicate the correspondence between the original and unified
          // extract vectors.
          val (updatedExtracts, extractMapping) =
            extractIndexPairs.foldLeft((extracts, Vector.empty[Int])):
              case ((seenExtracts, extractMapping), (extract, _)) => seenExtracts.indexOf(extract) match
                case index if index >= 0 => (seenExtracts, extractMapping :+ index)
                case _ => (seenExtracts :+ extract, extractMapping :+ seenExtracts.size)
          mapping += ((uniqueIndex, extractMapping))
          S((uniqueIndex, updatedExtracts))
        case N =>
          val fullMapping = (0 until extractIndexPairs.size).toVector
          val uniqueIndex = unique.size
          unique += pattern
          mapping += ((uniqueIndex, fullMapping))
          S((uniqueIndex, extractIndexPairs.map(_._1)))
    
    // Keep the following checks because it's easy to make mistakes here.
    assert(seenPatterns.size == unique.size)
    assert(patterns.size == mapping.size)
    (
      unique.result.map(pattern => (pattern, seenPatterns(pattern).extracts)),
      mapping.result
    )
  
  /** Compile the patterns that share the same outermost symbol. */
  private def compileSituationGroup(
      outermost: Situation.Outermost,
      situations: Vector[Situation]
  ) = trace(pre = s"Group of ${outermost.display}"):
    // Put the sub-patterns from each situation into rows, stack the
    // rows to form a matrix of sub-patterns, then draw each column.
    val patternColumns = (0 until outermost.arity).map: k =>
      trace(pre = s"Column $k"):
        val column = situations.map(_.subPatternExtractsPairs(k))
        log:
          val repr = column.iterator.zipWithIndex.map:
            case (element, situIndex) =>
              situations(situIndex).displaySubPattern(element)
          .mkString(" | ")
          s"Original: $repr"
        val (unifiedPatterns, indexMapping) = reconcilePatternSubExtracts(column)
        log:
          val repr = unifiedPatterns.iterator.map(_._1.display).mkString(" | ")
          s"Unique: $repr"
        log:
          val repr = indexMapping.iterator.zipWithIndex.map:
            case ((unifiedIndex, extractMapping), situIndex) =>
              val situ = situations(situIndex)
              val a = situ.displaySubPattern(column(situIndex))
              val (pattern, unifiedExtracts) = unifiedPatterns(unifiedIndex)
              val c =
                val tempNames = unifiedExtracts.iterator.zipWithIndex.map:
                  case (extract, index) => (extract, s"t${index.toSubscriptString}")
                .toVector
                Pattern.display(pattern, tempNames)
              // The extracts that are only used in the original pattern.
              val b = extractMapping.iterator.zipWithIndex.map:
                case (unifiedExtractIndex, originalExtractIndex) =>
                  // unifiedExtracts(unifiedExtractIndex) -> originalExtractIndex
                  s"${situ.getExtractName(originalExtractIndex)} = t${unifiedExtractIndex.toSubscriptString}"
              .mkString("(", ", ", ")")
              s"$situIndex. $a <- $b where $c"
          .mkString("\n")
          s"Mapping:\n$repr"
        // To construct a new match function for this column of patterns, we
        // need to place `extracts` into `Transformed` using temporary symbols
        // as `parameters`. If `extracts` is empty, it can be directly passed to
        // the new build.
        enqueueBuild:
          unifiedPatterns.map:
            case (pattern, extracts) =>
              if extracts.isEmpty then
                pattern
              else
                val parameters = (0 until extracts.size).map: i =>
                  VarSymbol(Ident(s"x${i.toSubscriptString}"))
                .toVector
                Transformed(pattern, extracts)(parameters, N)
    .toVector
  
  def buildMatchFunction(patterns: Vector[Pattern]): Unit = trace(
    pre = s"buildMatchFunction <<< ${patterns.displayAsUnion}",
    post = (res: Unit) => "buildMatchFunction >>> "
  ):
    val situations = enumerateSituations(patterns)
    log(s"Expanded situations:\n${situations.itemize}")
    situations.groupBy(_.outermost).foreach(compileSituationGroup)
    
  def compile(initialPatterns: Vector[Pattern]): Unit = trace(
    pre = s"compile <<< ${initialPatterns.displayAsUnion}",
    post = (res: Unit) => "compile >>> "
  ):
    buildQueue.enqueue(initialPatterns)
    while buildQueue.nonEmpty do
      log(s"Number of pending builds: ${buildQueue.size}")
      buildMatchFunction(buildQueue.dequeue())
  
  private val cache = MutMap.empty[Vector[Pattern], Opt[MatchFunction]]
  private val buildQueue = MutQueue.empty[Vector[Pattern]]
  
  private def enqueueBuild(inputPatterns: Vector[Pattern]): Unit =
    // Look up the input patterns in the cache using subsequence match.
    val cached = cache.iterator.collectFirst:
      Function.unlift:
        (patterns: Vector[Pattern], matchFunctionOpt: Opt[MatchFunction]) =>
          subsequence(patterns, inputPatterns).map(_ -> matchFunctionOpt)
    cached match
      case S((patterns, matchFunctionOpt)) => ()
      case N =>
        buildQueue.enqueue(inputPatterns)
        cache += (inputPatterns -> N)
  
  /** Search the cache for a `MatchFunction` that can match the given patterns.
   *  If an exact match is found, return that function; if one is found that
   *  is a superset for matching the given patterns, create a forwarded function;
   *  if none is found, return `None`. */
  private def findCachedMatchFunction(patterns: Ls[Pattern]): Opt[MatchFunction] =
    ???

object Compiler:
  /** Represents one case in `MatchFunction`. */
  class Situation(
      val originalPattern: SimplePattern,
      val extracts: Vector[Path],
      val parameters: Vector[VarSymbol],
      val transformer: Option[Term],
      /** Indicates the pattern from which this situation was expanded. */
      val index: Int
  ):
    require(extracts.size == parameters.size)
    
    val (
      /** The outermost constructor symbol. */
      outermost,
      /** All sub-patterns, in which every sub-pattern is paired with the
       *  subsequence formed by the `this.extracts` that appear only in that
       *  sub-pattern. */
      subPatternExtractsPairs,
      /** The extracts that refer to the `originalPattern` itself， represented
       *  by their index in `this.extracts`. */
      selfExtracts
    ) = Situation.mapExtractsToSubPatterns(originalPattern, extracts)
    
    /** for debug purposes */
    def getExtractName(index: Int) = parameters(index).name
    
    /** Display the original pattern with extract names for debug purposes. */
    def displayPattern: String =
      Pattern.display(originalPattern, extracts.zip(parameters.map(_.name)))
    
    /** Display elements from `subPatternExtractsPairs` for debug purposes.  */
    def displaySubPattern(subPatternWithExtracts: (Pattern, Vector[(Path, Int)])): String =
      val (subPattern, subExtracts) = subPatternWithExtracts
      val subExtractNamePairs = subExtracts.map:
        case (path, extractIndex) => (path, parameters(extractIndex).name)
      Pattern.display(subPattern, subExtractNamePairs)
  
  object Situation:
    opaque type Outermost = ClassLikeSymbol | Literal | Wildcard.type
    
    private def mapExtractsToSubPatterns(
        pattern: SimplePattern,
        extracts: Vector[Path]
    ): (
        Outermost, // the outermost constructor symbol
        Vector[(Pattern, Vector[(Path, Int)])], // sub-patterns and extracts
        Vector[Int] // self-extracts
    ) =
      val selfExtracts = extracts.iterator.zipWithIndex.collect:
        case (Path.End, index) => index
      .toVector
      pattern match
        case Constructor(symbol, subPatterns) =>
          val subPatternsWithExtracts = subPatterns.iterator.zipWithIndex.map:
            case (subPattern, subPatternIndex) =>
              val subExtracts = extracts.iterator.zipWithIndex.collect:
                case (Path.Select(`subPatternIndex`, rest), extractIndex) =>
                  (rest, extractIndex)
              .toVector
              (subPattern, subExtracts)
          .toVector
          (symbol, subPatternsWithExtracts, selfExtracts)
        case Constant(literal) => (literal, Vector.empty, selfExtracts)
        case Wildcard => (Wildcard, Vector.empty, selfExtracts)
    
    extension (outermost: Outermost)
      def arity: Int = outermost match
        case symbol: ClassSymbol => symbol.arity
        case _: ModuleSymbol | _: Literal | Wildcard => 0
      
      def display: String = outermost match
        case literal: Literal => literal.idStr
        case symbol: ClassLikeSymbol =>
          val arity = outermost.arity
          symbol.nme + (if arity == 0 then "" else
            "(_" + ", _".repeat(arity - 1) + ")")
        case Wildcard => "_"
    
    def apply(pattern: SimplePattern)(using index: Int): Situation =
      new Situation(pattern, Vector.empty, Vector.empty, N, index)
  
  extension (patterns: IterableOnce[Pattern])
    def display(sep: String): String =
      patterns.iterator.map(_.display).mkString(sep)
    def displayAsUnion = patterns.iterator.map(_.display).mkString(" | ")
    def displayAsSet: String = if patterns.knownSize == 0 then "{ }" else
      patterns.iterator.map(_.display).mkString("{", ", ", "}")
  
  extension (situations: IterableOnce[Situation])
    def itemize: String = if situations.knownSize == 0 then "<empty>" else
      situations.iterator.map: situ =>
        s"* ${situ.displayPattern} => ${situ.transformer.fold("<tupled>")(_.showDbg)} ~> ${situ.index}"
      .mkString("\n")
