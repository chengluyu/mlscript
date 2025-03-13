package hkmc2
package semantics
package ucs
package rp

import mlscript.utils.*, shorthands.*
import Message.MessageContext, syntax.Literal, syntax.Tree.Ident, Pattern.*
import collection.immutable.VectorBuilder
import collection.mutable.{Buffer, LinkedHashMap, Map as MutMap, Queue as MutQueue}
import Elaborator.*

class Compiler(elaborator: Elaborator)(using Ctx):
  import Compiler.*, elaborator.tl.*
  
  given State = elaborator.state
  given Raise = elaborator.raise
  
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
          
    /** Place `extracts` into `Transformed` using temporary symbols as
     *  `parameters` and make a tuple from the extracts. If `extracts` is empty,
     *  it just returns the pattern. */
    def makeTupleFromExtracts(extracts: Vector[Path]): Pattern =
      if extracts.isEmpty then
        pattern
      else
        val parameters = (0 until extracts.size).map: i =>
          VarSymbol(Ident(s"x${i.toSubscriptString}"))
        .toVector
        val tupleTerm = Term.Tup(
          parameters.iterator.map(symbol => PlainFld(symbol.ref())).toList
        )(syntax.Tree.Tup(Nil))
        Transformed(pattern, extracts)(parameters, S(tupleTerm))
  
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
    val seenPatterns = MutMap.empty[Pattern, (unifiedPatternIndex: Int, extracts: Vector[Path])]
    val unique = VectorBuilder[Pattern]()
    val mapping = VectorBuilder[(unifiedPatternIndex: Int, extractsMapping: Vector[Int])]()
    //                            from original to unified ^^^^^^^^^^^^^^^
    
    mapping.sizeHint(patterns.size)
    
    patterns.foreach:
      case (pattern, extractIndexPairs) => seenPatterns.updateWith(pattern):
        // TODO: The warning generated by destructing named tuples.
        case Some((unifiedPatternIndex, extracts)) =>
          // The unified extract vector might include more elements than each
          // original extract vector (a proper superset), so we create a mapping
          // to indicate the correspondence between the original and unified
          // extract vectors.
          val (updatedExtracts, extractsMapping) =
            extractIndexPairs.foldLeft((extracts, Vector.empty[Int])):
              case ((seenExtracts, extractsMapping), (extract, _)) => seenExtracts.indexOf(extract) match
                case index if index >= 0 => (seenExtracts, extractsMapping :+ index)
                case _ => (seenExtracts :+ extract, extractsMapping :+ seenExtracts.size)
          mapping += ((unifiedPatternIndex, extractsMapping))
          S((unifiedPatternIndex, updatedExtracts))
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
    val columnMatches = (0 until outermost.arity).map: k =>
      trace(pre = s"Column $k"):
        val patterns = situations.map(_.subPatternExtractsPairs(k))
        log(s"Original patterns: ${situations.displayPatternsAtColumn(k)}")
        val (unifiedPatterns, mapToUnified) = reconcilePatternSubExtracts(patterns)
        log(s"Unified patterns: ${unifiedPatterns.displayAsUnionWithTemporaryNames}")
        log:
          mapToUnified.iterator.zipWithIndex.map:
            case ((unifiedIndex, extractMapping), situIndex) =>
              val situ = situations(situIndex)
              val a = situ.displaySubPattern(patterns(situIndex))
              val (pattern, unifiedExtracts) = unifiedPatterns(unifiedIndex)
              val c = Pattern.displayWithTemporaryNames(pattern, unifiedExtracts)
              // The extracts that are only used in the original pattern.
              val b = extractMapping.iterator.zipWithIndex.map:
                case (unifiedExtractIndex, originalExtractIndex) =>
                  s"${situ.getExtractName(originalExtractIndex)} = t${unifiedExtractIndex.toSubscriptString}"
              .mkString("(", ", ", ")")
              s"$situIndex. $a ~> $c $b"
          .mkString("Mapping of extracts:\n", "\n", "")
        val matchFunction = scheduleBuild(unifiedPatterns.map(_.makeTupleFromExtracts(_)))
        log(s"Match function: ${matchFunction.id}")
        ColumnMatch(matchFunction, mapToUnified)
    .toVector
    MatchFunction.SituationGroup(outermost, situations, columnMatches)
  
  def buildMatchFunction(matchFunction: MatchFunction.Implemented): Unit = trace(
    pre = s"buildMatchFunction <<< ${matchFunction.inputPatterns.displayAsUnion}",
    post = (res: Unit) => "buildMatchFunction >>> "
  ):
    val situations = enumerateSituations(matchFunction.inputPatterns)
    log(s"Expanded situations:\n${situations.itemize}")
    val groups = LinkedHashMap.empty[Situation.Outermost, Vector[Situation]]
    situations.foreach: situation =>
      groups.updateWith(situation.outermost):
        case S(situations) => S(situations :+ situation)
        case N => S(Vector(situation))
    matchFunction.groups ++= groups.iterator.map(compileSituationGroup.tupled)
    
  def compile(initialPatterns: Vector[Pattern]): List[(BlockLocalSymbol, Term.Lam)] = trace(
    pre = s"compile <<< ${initialPatterns.displayAsUnion}",
    post = (res: List[(BlockLocalSymbol, Term.Lam)]) => "compile >>> "
  ):
    val entryPoint = scheduleBuild(initialPatterns)
    while queue.nonEmpty do
      log("")
      buildMatchFunction(queue.dequeue())
    // TODO: Currently, we create a compiler instance each time we found a
    // pattern synonyms. In the future, this will definitely be optimized, and
    // at that time, we will filter out all the used match functions.
    functions.valuesIterator.concat(forwards).map: matchFunction =>
      matchFunction.symbol -> matchFunction.makeLambda(elaborator)
    .toList
  
  private val functions = MutMap.empty[Vector[Pattern], MatchFunction]
  private val forwards = Buffer.empty[MatchFunction.Forwarded]
  private val queue = MutQueue.empty[MatchFunction.Implemented]
  
  /** Place the new build task in the queue. This method first checks whether
   *  there is an existing build task (either pending or completed) that can
   *  recognize the supersequence of the given patterns. If such a task exists,
   *  it creates a proxy match function that calls the existing match function
   *  and forwards the recognition result. */
  private def scheduleBuild(inputPatterns: Vector[Pattern]): MatchFunction =
    val id = functions.size
    val symbol = TempSymbol(N, s"match$id")
    findCachedMatchFunction(inputPatterns) match
      case S((indexMapping, target)) =>
        log(s"Forwarded ${inputPatterns.displayAsUnion} as $id to ${target.id}")
        val result = new MatchFunction.Forwarded(id, symbol, inputPatterns, target)
        forwards += result
        result
      case N =>
        log(s"Scheduled ${inputPatterns.displayAsUnion} as $id")
        val result = new MatchFunction.Implemented(id, symbol, inputPatterns)
        functions += (inputPatterns -> result)
        queue.enqueue(result)
        result
  
  /** Search the cache for a `MatchFunction` that can match the given patterns.
   *  If an exact match is found, return that function; if one is found that
   *  is a superset for matching the given patterns, create a forwarded function;
   *  if none is found, return `None`.
   *  @returns a pair of index mapping and the optional match function */
  private def findCachedMatchFunction(inputPatterns: Vector[Pattern]) =
    functions.iterator.collectFirst(Function.unlift((patterns, task) =>
      subsequence(patterns, inputPatterns).map(_ -> task)))

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
  
  class ColumnMatch(
      val matchFunction: MatchFunction,
      val patternMapping: Vector[(unifiedPatternIndex: Int, extractsMapping: Vector[Int])],
  )
  
  extension (patterns: IterableOnce[Pattern])
    def display(sep: String): String =
      patterns.iterator.map(_.display).mkString(sep)
    def displayAsUnion = patterns.iterator.map(_.display).mkString(" | ")
    def displayAsSet: String = if patterns.knownSize == 0 then "{ }" else
      patterns.iterator.map(_.display).mkString("{", ", ", "}")
  
  extension (patternExtractsPair: IterableOnce[(Pattern, Vector[Path])])
    def displayAsUnionWithTemporaryNames: String =
      patternExtractsPair.iterator.map:
        Pattern.displayWithTemporaryNames.tupled
      .mkString(" | ")
  
  extension (situations: IterableOnce[Situation])
    def itemize: String = if situations.knownSize == 0 then "<empty>" else
      situations.iterator.map: situ =>
        "* " + situ.displayPattern +
          situ.transformer.fold("")(" => " + _.showDbg) +
          s" ~> ${situ.index}"
      .mkString("\n")
    
    def displayPatternsAtColumn(index: Int) =
      situations.iterator.map: situation =>
        situation.displaySubPattern(situation.subPatternExtractsPairs(index))
      .mkString(" | ")
