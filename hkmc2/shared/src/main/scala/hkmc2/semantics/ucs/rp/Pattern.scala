package hkmc2
package semantics
package ucs
package rp

import syntax.Literal

// TODO: Add locations during the elaboration stage.
enum Pattern:
  case Constructor(symbol: ClassLikeSymbol, subPatterns: Vector[Pattern])
  /** When `symbol` is `PatternSymbol`, it indicates using other patterns, and
   *  `patternParameters` must contain exactly `symbol.patternParameterCount`
   *  patterns. When `symbol` is `VarSymbol`, it indicates using some pattern
   *  parameters, and `patternParameters` must be empty. */
  case NonTerminal(
      symbol: PatternSymbol | VarSymbol,
      patternParameters: List[Pattern],
      @deprecated("We might not extract things from pattern synonyms in this way.")
      subPatterns: Vector[Pattern]
  )
  case Alternative(first: Pattern, second: Pattern)
  case Constant(literal: Literal)
  case Wildcard
  /** Represents a pattern with a transformation term, where `extracts` points
   *  to the sub-terms in the scrutinee that need to be extracted and
   *  corresponds one-to-one with `parameters`, while `term` refers to the
   *  extracted sub-terms via the `Symbol`s in `parameters`.
   * 
   *  The `parameters` and `term` are both placed in the second parameter list
   *  so that they do not affect the case class's hash code.
   * 
   *  @param transformer `None` if a tuple is directly created from `parameters`,
   *  `Some[Term]` if the user has written a transformation term.
   */
  case Transformed
      (pattern: Pattern, extracts: Vector[Pattern.Path])
      (val parameters: Vector[VarSymbol], val transformer: Option[Term])
  
  def display: String = Pattern.display(this, Vector.empty)
  
  /** Obtain the number of sub-terms extracted from the scrutinee if matched by
   *  this pattern. */
  @deprecated("We might not extract things from pattern synonyms in this way.")
  def extractCount: Int = this match
    case Constructor(_, _) => 0
    case NonTerminal(symbol, patternParameters, subPatterns) => 0
    case Alternative(first, second) =>
      val firstExtractCount = first.extractCount
      val secondExtractCount = second.extractCount
      if firstExtractCount == secondExtractCount then firstExtractCount else 0
    case Constant(_) => 0
    case Wildcard => 0
    case pattern: Transformed =>
      // It seems that there is no way to determine at compile time what `term`
      // returns, so it is impossible to determine how many extracts the pattern
      // can produce.
      if pattern.transformer.isEmpty then pattern.parameters.size else 1

import Pattern.*

type SimplePattern = Constructor | Constant | Wildcard.type

object Pattern:
  enum Path:
    case End
    case Select(index: Int, rest: Path)
    case Diverge(first: Path, second: Path)
    
    override def toString(): String = this match
      case End => "$"
      case Select(index, rest) => s"$index -> $rest"
      case Diverge(first, second) =>s"( $first | $second )"
  
  private def display(
      patterns: Vector[Pattern],
      paths: Vector[(Path, String)]
  ): Iterator[String] =
    patterns.iterator.zipWithIndex.map:
      case (subPattern, index) =>
        val selectedPaths = paths.collect:
          case (Path.Select(`index`, rest), name) => (rest, name)
        display(subPattern, selectedPaths)
  
  // TODO: Move this function to `utils`. Why can't it overload `mkString`?
  extension (iterator: Iterator[String])
    def joined(start: String, sep: String, end: String, empty: => String): String =
      if iterator.hasNext then iterator.iterator.mkString(start, sep, end) else empty
      
  private[rp] def displayWithTemporaryNames(pattern: Pattern, extracts: Vector[Path]): String =
    val extractsWithTemporaryNames = extracts.iterator.zipWithIndex.map:
      case (extract, index) => (extract, s"t${index.toSubscriptString}")
    .toVector
    display(pattern, extractsWithTemporaryNames)
  
  private[rp] def display(pattern: Pattern, paths: Vector[(Path, String)]): String =
    val make =
      val endedPaths = paths.iterator.collect:
        case (Path.End, name) => name
      if endedPaths.hasNext then
        val tail = endedPaths.mkString(" as ")
        (par: Boolean) => (input: String) => input match
          case "_" => tail
          case _ if par => "(" + input + ") as " + tail
          case _ => input + " as " + tail
      else
        Function.const(identity[String])
    pattern match
      case Constructor(symbol, subPatterns) => make(false):
        symbol.nme + (symbol match
        case symbol: ClassSymbol if symbol.arity == 0 => ""
        case _: ClassSymbol =>
          display(subPatterns, paths).mkString("(", ", ", ")")
        case _: ModuleSymbol => "")
      case NonTerminal(symbol: PatternSymbol, patternParameters, subPatterns) => make(false):
        symbol.nme + (patternParameters.iterator.map(display(_, Vector.empty))
          .concat(display(subPatterns, paths)).joined("(", ", ", ")", ""))
      case NonTerminal(symbol: VarSymbol, _, subPatterns) => make(false):
        symbol.nme + (if subPatterns.isEmpty then "" else
          display(subPatterns, paths).joined("(", ", ", ")", ""))
      case Alternative(first, second) => make(true):
        val firstPaths = paths.collect:
          case (Path.Diverge(first, _), name) => (first, name)
        val secondPaths = paths.collect:
          case (Path.Diverge(_, second), name) => (second, name)
        display(first, firstPaths) + " | " + display(second, secondPaths)
      case Constant(literal) => make(false)(literal.idStr)
      case Wildcard => make(false)("_")
      case pattern @ Transformed(innerPattern, extracts) => make(true):
        require(extracts.size == pattern.parameters.size)
        display(innerPattern, extracts.zip(pattern.parameters.map(_.name)))

