package hkmc2
package syntax

import mlscript.utils._, shorthands._


/** Type of general Tokens */
sealed abstract class Token:
  def describe(using context: Context): Str = this match
    case SPACE => "space"
    case COMMA => "comma"
    case SEMI => "semicolon"
    case NEWLINE => "newline"
    case ERROR => "error"
    case QUOTE => "quote"
    case LITVAL(value) => "literal"
    case IDENT(name, symbolic) =>
      if context.keywords.contains(name) then s"'$name' keyword"
      else if symbolic then "operator" else "identifier"
    case SELECT(name) => "selector"
    case OPEN_BRACKET(k) => s"opening ${k.name}"
    case CLOSE_BRACKET(k) => s"closing ${k.name}"
    case COMMENT(text) => "comment"

case object SPACE extends Token
case object COMMA extends Token
case object SEMI extends Token
case object NEWLINE extends Token
case object ERROR extends Token
case object QUOTE extends Token
final case class LITVAL(value: Literal) extends Token
final case class IDENT(name: String, symbolic: Bool) extends Token
final case class SELECT(name: String) extends Token
final case class OPEN_BRACKET(k: BracketKind) extends Token
final case class CLOSE_BRACKET(k: BracketKind) extends Token
final case class COMMENT(text: String) extends Token


sealed abstract class BracketKind:
  import BracketKind._
  lazy val (beg, end) = this match
    case Round => "(" -> ")"
    case Curly => "{" -> "}"
    case Square => "[" -> "]"
    case Angle => "‹" -> "›"
  def name: Str = this match
    case Round => "parenthesis"
    case Curly => "curly brace"
    case Square => "square bracket"
    case Angle => "angle bracket"

object BracketKind:
  case object Round extends BracketKind
  case object Curly extends BracketKind
  case object Square extends BracketKind
  case object Angle extends BracketKind
  
  def unapply(c: Char): Opt[Either[BracketKind, BracketKind]] = c `|>?` :
    case '(' => Left(Round)
    case ')' => Right(Round)
    case '{' => Left(Curly)
    case '}' => Right(Curly)
    case '[' => Left(Square)
    case ']' => Right(Square)
    case '‹' => Left(Angle)
    case '›' => Right(Angle)

