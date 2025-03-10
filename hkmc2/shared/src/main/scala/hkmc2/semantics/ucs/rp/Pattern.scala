package hkmc2
package semantics
package ucs
package rp

enum Pattern:
  case Constructor(symbol: ClassLikeSymbol, parameters: Vector[Pattern])
  case NonTerminal(
      symbol: PatternSymbol | (LocalSymbol & NamedSymbol),
      patternParameters: List[Pattern],
      extractions: Vector[Pattern]
  )
  case Alternative(first: Pattern, second: Pattern)
  case Constant(literal: syntax.Literal)
  case Wildcard
  case Transformed
      (pattern: Pattern, parameters: Vector[Pattern.Path])
      (val symbols: Vector[LocalSymbol & NamedSymbol], val term: Term)

object Pattern:
  enum Path:
    case End
    case Select(index: Int, rest: Path)
    case Diverge(first: Path, second: Path)
    
    override def toString(): String = this match
      case End => "$"
      case Select(index, rest) => s"$index -> $rest"
      case Diverge(first, second) =>s"( $first | $second )"

