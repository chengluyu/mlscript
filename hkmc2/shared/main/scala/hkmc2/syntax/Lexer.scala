package hkmc2
package syntax

import scala.annotation.tailrec
import mlscript._
import utils._, shorthands._

import Message.MessageContext
import Diagnostic.Source.{Lexing, Parsing}

import Lexer._
import Literal.{IntLit, DecLit, StrLit}


class Lexer(origin: Origin, raise: Raise, dbg: Bool):
  import Lexer.*
  
  val bytes: Array[Char] = origin.fph.blockStr.toArray
  private val length = bytes.length
  type State = Int
  
  
  def matches(i: Int, syntax: Str, start: Int): Bool =
    if start < syntax.length && i + start < length && bytes(i + start) === syntax(start) then matches(i, syntax, start + 1)
    else start >= syntax.length
  
  /* // TODO remove (unused)
  private val isNonStickyKeywordChar = Set(
    ',',
    ':',
    ';',
  )
  */
  
  private val isSymKeyword = Set(
    // "->",
    "=>",
    "=",
    ":",
    ";",
    // ",",
    "#",
    "`"
    // ".",
    // "<",
    // ">",
  )
  
  // TODO rm
  private val isAlphaOp = Set[Str](
    // "with",
    // "and",
    // "or",
    // "is",
    // "as",
  )
  
  @tailrec final
  def takeWhile(i: Int, cur: Ls[Char] = Nil)(pred: Char => Bool): (Str, Int) =
    if i < length && pred(bytes(i)) then takeWhile(i + 1, bytes(i) :: cur)(pred)
    else (cur.reverseIterator.mkString, i)

  final def num(i: Int): (Literal, Int) =
    def test(i: Int, p: Char => Bool): Bool = i < length && p(bytes(i))
    def zero: IntLit = IntLit(BigInt(0))
    /** Take a sequence of digits interleaved with underscores. */
    def takeDigits(i: Int, pred: Char => Bool): (Opt[Str], Int) =
      @tailrec def rec(i: Int, acc: Ls[Char], firstSep: Bool, lastSep: Bool): (Str, Bool, Bool, Int) =
        if i < length then
          val c = bytes(i)
          if pred(c) then rec(i + 1, c :: acc, firstSep, false)
          else if c === '_' then rec(i + 1, acc, acc.isEmpty, true)
          else (acc.reverseIterator.mkString, firstSep, lastSep, i)
        else (acc.reverseIterator.mkString, firstSep, lastSep, i)
      val (str, firstSep, lastSep, j) = rec(i, Nil, false, false)
      if firstSep then
        raise(WarningReport(
          msg"Leading separator is not allowed" -> S(loc(i - 1, i)) :: Nil,
          newDefs = true, source = Lexing))
      if lastSep then
        raise(WarningReport(
          msg"Trailing separator is not allowed" -> S(loc(j - 1, j)) :: Nil,
          newDefs = true, source = Lexing))
      (if str.isEmpty then N else S(str), j)
    /** Take an integer and coverts to `BigInt`. Also checks if it is empty. */
    def integer(i: Int, radix: Int, desc: Str, pred: Char => Bool): (IntLit, Int) =
      takeDigits(i, pred) match
        case (N, j) =>
          raise(ErrorReport(msg"Expect at least one $desc digit" -> S(loc(i, i + 2)) :: Nil,
            newDefs = true, source = Lexing))
          (zero, j)
        case (S(str), j) => (IntLit(BigInt(str, radix)), j)
    def isDecimalStart(ch: Char) = ch === '.' || ch === 'e' || ch === 'E'
    /** Take a fraction part with an optional exponent part. Call at periods. */
    def decimal(i: Int, integral: Str): (DecLit, Int) =
      val (fraction, j) = if test(i, _ === '.') then
        takeDigits(i + 1, isDigit) match
          case (N, j) =>
            raise(ErrorReport(msg"Expect at least one digit after the decimal point" -> S(loc(i + 1, i + 2)) :: Nil,
              newDefs = true, source = Lexing))
            ("", j)
          case (S(digits), j) => ("." + digits, j)
      else ("", i)
      val (exponent, k) = if test(j, ch => ch === 'e' || ch === 'E') then
        val (sign, k) = if test(j + 1, ch => ch === '+' || ch === '-') then
          (bytes(j + 1), j + 2)
        else
          ('+', j + 1)
        takeDigits(k, isDigit) match
          case (N, l) =>
            raise(ErrorReport(msg"Expect at least one digit after the exponent sign" -> S(loc(l - 1, l)) :: Nil,
              newDefs = true, source = Lexing))
            ("", l)
          case (S(digits), l) => ("E" + sign + digits, l)
      else
        ("", j)
      (DecLit(BigDecimal(integral + fraction + exponent)), k)
    if i < length then
      bytes(i) match
        case '0' if i + 1 < length => bytes(i + 1) match
          case 'x' => integer(i + 2, 16, "hexadecimal", isHexDigit)
          case 'o' => integer(i + 2, 8, "octal", isOctDigit)
          case 'b' => integer(i + 2, 2, "binary", isBinDigit)
          case '.' | 'E' | 'e' => decimal(i + 1, "0")
          case _ => integer(i, 10, "decimal", isDigit)
        case '0' => (zero, i + 1)
        case _ => takeDigits(i, isDigit) match
          case (N, j) =>
            raise(ErrorReport(msg"Expect a numeric literal" -> S(loc(i, i + 1)) :: Nil,
              newDefs = true, source = Lexing))
            (zero, i)
          case (S(integral), j) =>
            if j < length && isDecimalStart(bytes(j)) then decimal(j, integral)
            else (IntLit(BigInt(integral)), j)
    else
      raise(ErrorReport(msg"Expect a numeric literal instead of end of input" -> S(loc(i, i + 1)) :: Nil,
        newDefs = true, source = Lexing))
      (zero, i)

  // * Check the end of a string (either single quotation or triple quotation)
  final def closeStr(i: Int, isTriple: Bool): Int =
    if !isTriple && bytes.lift(i) === Some('"') then i + 1
    else if isTriple && matches(i, "\"\"\"", 0) then i + 3
    else
      raise(ErrorReport(msg"unclosed quotation mark" -> S(loc(i, i + 1)) :: Nil, newDefs = true, source = Lexing))
      i

  @tailrec final
  def str(i: Int, escapeMode: Bool, cur: Ls[Char] = Nil)(implicit triple: Bool): (Str, Int) =
    if escapeMode then
      if i < length then
        bytes(i) match
          case '\\' => str(i + 1, false, '\\' :: cur)
          case '"' => str(i + 1, false, '"' :: cur)
          case 'n' => str(i + 1, false, '\n' :: cur)
          case 't' => str(i + 1, false, '\t' :: cur)
          case 'r' => str(i + 1, false, '\r' :: cur)
          case 'b' => str(i + 1, false, '\b' :: cur)
          case 'f' => str(i + 1, false, '\f' :: cur)
          case ch =>
            raise(WarningReport(msg"Found invalid escape character" -> S(loc(i, i + 1)) :: Nil,
              newDefs = true, source = Lexing))
            str(i + 1, false, ch :: cur)
      else
        raise(ErrorReport(msg"Expect an escape character" -> S(loc(i, i + 1)) :: Nil,
          newDefs = true, source = Lexing))
        (cur.reverseIterator.mkString, i)
    else if triple then
      if i < length then
        bytes(i) match
          case '"' =>
            if matches(i, "\"\"\"", 0) && !matches(i + 1, "\"\"\"", 0) then // Find the last """
              (cur.reverseIterator.mkString, i)
            else str(i + 1, false, '"' :: cur)
          case ch => str(i + 1, false, ch :: cur)
      else (cur.reverseIterator.mkString, i)
    else
      if i < length then
        bytes(i) match
          case '\\' => str(i + 1, true, cur)
          case '"' | '\n' => (cur.reverseIterator.mkString, i)
          case ch => str(i + 1, false, ch :: cur)
      else
        (cur.reverseIterator.mkString, i)
  
  def loc(start: Int, end: Int): Loc = Loc(start, end, origin)
  
  @tailrec final
  def lex(i: Int, ind: Ls[Int], acc: Ls[TokLoc]): Ls[TokLoc] = if i >= length then acc.reverse else
    
    val c = bytes(i)
    
    def pe(msg: Message): Unit =
      // raise(ParseError(false, msg -> S(loc(i, i + 1)) :: Nil))
      raise(ErrorReport(msg -> S(loc(i, i + 1)) :: Nil, newDefs = true, source = Lexing))
    
    inline def go(j: Int, tok: Token) = lex(j, ind, (tok, loc(i, j)) :: acc)
    inline def next(j: Int, tok: Token) = (tok, loc(i, j)) :: acc
    
    c match
      case ' ' | '\r' | '\t' =>
        val (_, j) = takeWhile(i)(isWhiteSpace)
        // go(j, SPACE)
        lex(j, ind, next(j, SPACE))
      case ',' =>
        val j = i + 1
        // go(j, COMMA)
        lex(j, ind, next(j, COMMA))
      case '`' =>
        lex(i + 1, ind, next(i + 1, QUOTE))
      case ';' =>
        val j = i + 1
        lex(j, ind, next(j, SEMI))
      case '"' =>
        val (chars, k) = str(i + 1, false)(false)
        val k2 = closeStr(k, false)
        // go(k2, LITVAL(StrLit(chars)))
        lex(k2, ind, next(k2, LITVAL(StrLit(chars))))
      case '/' if bytes.lift(i + 1).contains('/') =>
        val j = i + 2
        val (txt, k) =
          takeWhile(j)(c => c =/= '\n')
        // go(k, COMMENT(txt))
        lex(k, ind, next(k, COMMENT(txt)))
      case '/' if bytes.lift(i + 1).contains('*') => // multiple-line comment
        val j = i + 2
        var prev1 = '/'; var prev2 = '*'
        val (txt, k) =
          takeWhile(j)(c => {
            val res = prev1 =/= '*' || prev2 =/= '/'
            prev1 = prev2; prev2 = c
            res
          })
        // go(k, COMMENT(txt.dropRight(2)))
        lex(k, ind, next(k, COMMENT(txt.dropRight(2))))
      // case BracketKind(Left(k)) => go(i + 1, OPEN_BRACKET(k))
      // case BracketKind(Right(k)) => go(i + 1, CLOSE_BRACKET(k))
      case BracketKind(Left(k)) => lex(i + 1, ind, next(i + 1, OPEN_BRACKET(k)))
      case BracketKind(Right(k)) => lex(i + 1, ind, next(i + 1, CLOSE_BRACKET(k)))
      case '\n' =>
        val j = i + 1
        val (space, k) =
          takeWhile(j)(c => c === ' ' || c === '\n')
        val nextInd = space.reverseIterator.takeWhile(_ =/= '\n').size
        if ind.headOption.forall(_ < nextInd) && nextInd > 0 then
          lex(k, nextInd :: ind, (SPACE, loc(j, k)) :: acc)
        else
          val newIndBase = ind.dropWhile(_ > nextInd)
          val droppedNum = ind.size - newIndBase.size
          val hasNewIndent = newIndBase.headOption.forall(_ < nextInd) && nextInd > 0
          val newInd = if hasNewIndent then nextInd :: newIndBase else newIndBase
          if dbg then
            println("dbg: " + bytes.drop(i).take(10).map(escapeChar).mkString+"...")
            println((ind, nextInd, newIndBase, droppedNum, hasNewIndent, newInd))
          lex(k, newInd,
            // if droppedNum > 0 then {
            //   if hasNewIndent then (INDENT, loc(j, k))
            //   else (NEWLINE, loc(i, k))
            // } :: List.fill(droppedNum)((DEINDENT, loc(j-1, k))) ::: acc
            // else
            (NEWLINE, loc(i, k)) :: acc
          )
      case _ if isIdentFirstChar(c) =>
        val (n, j) = takeWhile(i)(isIdentChar)
        // go(j, if (keywords.contains(n)) KEYWRD(n) else IDENT(n, isAlphaOp(n)))
        lex(j, ind, next(j,
            // if keywords.contains(n) then KEYWRD(n) else IDENT(n, isAlphaOp(n))
            IDENT(n, isAlphaOp(n))
          ))
      case _ if isOpChar(c) =>
        val (n, j) = takeWhile(i)(isOpChar)
        if n === "." && j < length then
          val nc = bytes(j)
          if isIdentFirstChar(nc) then
            val (name, k) = takeWhile(j)(isIdentChar)
            // go(k, SELECT(name))
            lex(k, ind, next(k, SELECT(name)))
          else if
            // The first character is '0' and the next character is not a digit
            (nc === '0' && !(j + 1 < length && isDigit(bytes(j + 1)))) ||
            ('0' < nc && nc <= '9') // The first character is a digit other than '0'
          then
            val (name, k) = takeWhile(j)(isDigit)
            // go(k, SELECT(name))
            lex(k, ind, next(k, SELECT(name)))
          else lex(j, ind, next(j,
              // if isSymKeyword.contains(n) then KEYWRD(n) else IDENT(n, true)
              IDENT(n, true)
            ))
        // else go(j, if (isSymKeyword.contains(n)) KEYWRD(n) else IDENT(n, true))
        else lex(j, ind, next(j,
            // if isSymKeyword.contains(n) then KEYWRD(n) else IDENT(n, true)
            IDENT(n, true)
          ))
      case _ if isDigit(c) =>
        val (lit, j) = num(i)
        // go(j, LITVAL(IntLit(BigInt(str))))
        lex(j, ind, next(j, LITVAL(lit)))
      case _ =>
        pe(msg"unexpected character '${escapeChar(c)}'")
        // go(i + 1, ERROR)
        lex(i + 1, ind, next(i + 1, ERROR))
  
  lazy val tokens: Ls[Token -> Loc] = lex(0, Nil, Nil)

object Lexer:
  
  type TokLoc = (Token, Loc)
  
  val keywords: Set[Str] = Set(
    "if",
    "then",
    "else",
    "case",
    "fun",
    "val",
    "var",
    // "is",
    // "as",
    "of",
    // "and",
    // "or",
    "let",
    "rec",
    "in",
    // "any",
    // "all",
    "mut",
    "set",
    "do",
    "while",
    "declare",
    "class",
    "trait",
    "mixin",
    "interface",
    "extends",
    "override",
    "super",
    "new",
    "namespace",
    "module",
    "type",
    "where",
    "forall",
    "exists",
    "in",
    "out",
    "null",
    "undefined",
    "abstract",
    "constructor",
    "virtual"
  )
  
  def printToken(tl: TokLoc): Str = tl match
    case (SPACE, _) => " "
    case (COMMA, _) => ","
    case (SEMI, _) => ";"
    case (NEWLINE, _) => "↵"
    case (ERROR, _) => "<error>"
    case (QUOTE, _) => "`"
    case (LITVAL(lv), _) => lv.idStr
    // case (KEYWRD(name: String), _) => "#" + name
    case (IDENT(name: String, symbolic: Bool), _) => name
    case (SELECT(name: String), _) => "." + name
    case (OPEN_BRACKET(k), _) => k.beg
    case (CLOSE_BRACKET(k), _) => k.end
    case (COMMENT(text: String), _) => "/*" + text + "*/"
  def printTokens(ts: Ls[TokLoc]): Str =
    ts.iterator.map(printToken).mkString("|", "|", "|")
  
  def escapeChar(ch: Char): String = ch match
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if ch.isControl
      then "\\0" + Integer.toOctalString(ch.toInt) 
      else String.valueOf(ch)

  val isOpChar = Set(
    '!', '#', '%', '&', '*', '+', '-', '/', ':', '<', '=', '>', '?', '@', '\\', '^', '|', '~' , '.',
    // ',', 
    // ';'
  )

  def isOp(name: String): Bool = name.forall(isOpChar)

  def isIdentFirstChar(c: Char): Bool =
    c.isLetter || c === '_' || c === '\'' || c === '$'
  def isIdentChar(c: Char): Bool =
    isIdentFirstChar(c) || isDigit(c) || c === '\''
  def isHexDigit(c: Char): Bool =
    isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  def isOctDigit(c: Char): Bool =
    c >= '0' && c <= '7'
  def isBinDigit(c: Char): Bool =
    c === '0' || c === '1'
  def isDigit(c: Char): Bool =
    c >= '0' && c <= '9'
  def isWhiteSpace(c: Char): Bool =
    c === ' ' | c === '\n' | c === '\r' | c === '\t'
