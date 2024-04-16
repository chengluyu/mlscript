package hkmc2
package syntax

import scala.util.boundary
import sourcecode.{Name, Line}
import mlscript.utils.*, shorthands.*
import hkmc2.Message._
import BracketKind._

import Parser.*
import hkmc2.Diagnostic.Kind
import scala.annotation.tailrec


object Parser:
  
  type TokLoc = (Token, Loc)
  
  type LTL = Ls[TokLoc]
  
  private val MinPrec = 0
  private val NoElsePrec = MinPrec + 1
  
  private val prec: Map[Char,Int] =
    List(
      "", // `of` rhs
      ",",
      // ^ for keywords
      // ";",
      "=",
      "@",
      ":",
      "|",
      "/ \\",
      "^",
      "&",
      // "= !",
      "!",
      "< >",
      "+ -",
      // "* / %",
      "* %",
      "", // Precedence of application
      ".",
    ).zipWithIndex.flatMap {
      // case (cs, i) => cs.filterNot(_ === ' ').map(_ -> (i + Keyword.maxPrec.get))
      // Well, unable to have a constant `maxPrec` becuase keywords can be introduced.
      case (cs, i) => cs.filterNot(_ === ' ').map(_ -> (i + 2))
    }.toMap.withDefaultValue(Int.MaxValue)
  
  // private val CommaPrec = prec(',')
  private val CommaPrec = 0
  private val CommaPrecNext = CommaPrec + 1
  private val AppPrec = prec('.') - 1
  
  final def opCharPrec(opChar: Char): Int = prec(opChar)
  final def opPrec(opStr: Str): (Int, Int) = opStr match {
    case "+." | "-." | "*." =>
      (prec(opStr.head), prec(opStr.head))
    case _ if opStr.exists(_.isLetter) =>
      // (Keyword.maxPrec.get, Keyword.maxPrec.get)
      (Int.MaxValue, Int.MaxValue)
    case _ =>
      val r = opStr.last
      (prec(opStr.head), prec(r) - (if r === '@' || r === '/' || r === ',' || r === ':' then 1 else 0))
  }
  object KW:
    private val BASE = 2

    val `::=` = Keyword("::=", Some(BASE), Some(BASE))
    val `~>` = Keyword("~>", Some(BASE), Some(BASE))
    val `keyword` = Keyword("keyword", None, None)
    val `define` = Keyword("define", None, None)
    val `=>` = Keyword("=>", Some(BASE + 1), Some(BASE))

    def unapply(t: Token)(using context: Context): Option[Keyword] = t match
      case IDENT(name, _) => context.keywords.get(name)
      case _ => None
  
  object OP:
    def unapply(t: Token)(using context: Context): Option[String] = t match
      case IDENT(name, true) if !context.keywords.contains(name) => Some(name)
      case _ => None
end Parser

import Parser._

abstract class Parser(
  origin: Origin,
  tokens: Ls[TokLoc],
  raiseFun: Diagnostic => Unit,
  val dbg: Bool,
):
  protected def doPrintDbg(msg: => Str): Unit
  protected def printDbg(msg: => Any): Unit =
    doPrintDbg("│ " * this.indent + msg)
  
  protected var indent = 0
  private var _cur: Ls[TokLoc] = tokens
  
  private def wrap[R](args: => Any)(mkRes: => R)(implicit l: Line, n: Name): R =
    printDbg(s"@ ${n.value}${args match {
      case it: Iterable[_] => it.mkString("(", ",", ")")
      case _: Product => args
      case _ => s"($args)"
    }}    [at syntax/Parser.scala:${l.value}]")
    val res = try
      indent += 1
      mkRes
    finally indent -= 1
    printDbg(s"= $res")
    res
  
  def resetCur(newCur: Ls[TokLoc]): Unit =
    _cur = newCur
    // _modifiersCache = ModifierSet.empty
  
  private lazy val lastLoc =
    tokens.lastOption.map(_._2.right)//.orElse(fallbackLoc)
  
  private def summarizeCur =
    Lexer.printTokens(_cur.take(5)) + (if _cur.sizeIs > 5 then "..." else "")
  
  private def cur(implicit l: Line, n: Name) =
    if dbg then printDbg(s"? ${n.value}\t\tinspects ${summarizeCur}    [at syntax/Parser.scala:${l.value}]")
    while !_cur.isEmpty && (_cur.head._1 match {
      case COMMENT(_) => true
      case _ => false
    }) do consume
    _cur
  
  final def consume(implicit l: Line, n: Name): Unit =
    if dbg then printDbg(s"! ${n.value}\t\tconsumes ${Lexer.printTokens(_cur.take(1))}    [at syntax/Parser.scala:${l.value}]")
    resetCur(_cur.tailOption.getOrElse(Nil)) // FIXME throw error if empty?
  
  private def yeetSpaces(using Line, Name): Ls[TokLoc] =
    cur.dropWhile(tkloc =>
      (tkloc._1 === SPACE
      || tkloc._1.isInstanceOf[COMMENT] // TODO properly retrieve and store all comments in AST?
      ) && { consume; true })
  
  // final def raise(mkDiag: => Diagnostic)(implicit fe: FoundErr = false): Unit =
  //   if (!foundErr) raiseFun(mkDiag)
  final def raise(mkDiag: => Diagnostic): Unit =
    raiseFun(mkDiag)
  
  private def errExpr =
    Tree.Empty // TODO FIXME produce error term instead
  
  final def err(msgs: Ls[Message -> Opt[Loc]])(implicit l: Line, n: Name): Unit =
    printDbg(s"Error    [at syntax/Parser.scala:${l.value}]")
    raise(ErrorReport(msgs, newDefs = true, source = Diagnostic.Source.Parsing))

  final def warn(msgs: Ls[Message -> Opt[Loc]])(implicit l: Line, n: Name): Unit =
    printDbg(s"Warning    [at syntax/Parser.scala:${l.value}]")
    raise(WarningReport(msgs, newDefs = true, source = Diagnostic.Source.Parsing))

  // Report unexpected tokens in various ways.

  def unexpected[A](expected: String, actual: String, loc: Option[Loc], res: A): A =
    err((msg"Expected $expected; found $actual instead" -> loc :: Nil)); res

  def unexpected(expected: String, tok: Token, loc: Loc)(using Context): Tree =
    unexpected(expected, tok.describe, S(loc))

  def unexpected(expected: String, actual: String, loc: Option[Loc]): Tree =
    unexpected(expected, actual, loc, errExpr)

  def unexpected(expected: String): Tree = unexpected(expected, "end of input", lastLoc)
  
  final def parseAll[R](parser: => R)(using Context): R =
    val res = parser
    cur match
      case c @ (tk, tkl) :: _ =>
        val (relevantToken, rl) = c.dropWhile(_._1 === SPACE).headOption.getOrElse(tk, tkl)
        err(msg"Expected end of input; found ${relevantToken.describe} instead" -> S(rl) :: Nil)
      case Nil => ()
    res
  
  final def concludeWith[R](f: this.type => R)(using Context): R =
    val res = f(this)
    cur.dropWhile(tk => (tk._1 === SPACE) && { consume; true }) match
      case c @ (tk, tkl) :: _ =>
        val (relevantToken, rl) = c.dropWhile(_._1 === SPACE).headOption.getOrElse(tk, tkl)
        err(msg"Unexpected ${relevantToken.describe} here" -> S(rl) :: Nil)
      case Nil => ()
    printDbg(s"Concluded with $res")
    res

  /** Expect the next token is an identifier */
  def expectIdent[A](previous: String, failure: => A)(success: (IDENT, Loc) => A)(using Context): A =
    yeetSpaces match
    case (tok @ (id: IDENT), loc) :: _ => consume; success(id, loc)
    case (tok, loc) :: _ => unexpected(s"an identifier after $previous", tok.describe, S(loc), failure)
    case Nil => unexpected(s"an identifier after $previous", "end of input", lastLoc, failure)

  /** Expect the next token is the given keyword */
  def expectKeyword[A](keyword: Keyword, previous: String, failure: => A)(success: (Token, Loc) => A)(using Context): A =
    yeetSpaces match
    case (tok @ KW(keyword), loc) :: _ => consume; success(tok, loc)
    case (tok, loc) :: _ => unexpected(s"${keyword.name} after $previous", tok.describe, S(loc), failure)
    case Nil => unexpected(s"${keyword.name} after $previous", "end of input", lastLoc, failure)
  
  def block(using context: Context): WithContext[List[Tree]] = wrap("block"):
    cur match
    case Nil => WithContext(Nil, context)
    case (SPACE, _) :: _ => consume; block
    case (tok @ KW(KW.`keyword`), loc) :: _ =>
      consume; blockCont(using parseKeyword)
    case (tok @ KW(KW.`define`), loc) :: _ =>
      consume; blockCont(using parseDefine)
    case (tok @ (id: IDENT), loc) :: _ =>
      val head = tryKwAlt(id, loc, context.prefixRules, errExpr):
        tryIdentAlt(id, loc, context.prefixRules, errExpr):
          tryExprAlt(id, loc, context.prefixRules, errExpr):
            unexpected(s"${context.prefixRules.whatComesAfter} after ${context.prefixRules.name}", id.describe, S(loc), errExpr)
      blockCont.map(head :: _)
    case (tok, loc) :: _ =>
      val head = tryExprThenEmpty(CommaPrecNext, tok, loc, context.prefixRules)
        .getOrElse(errExpr)
      blockCont.map(head :: _)
  
  def blockCont(using context: Context): WithContext[List[Tree]] =
    yeetSpaces match
      case (COMMA | SEMI, _) :: _ =>
        consume
        yeetSpaces match
          case (NEWLINE, _) :: _ => consume
          case _ => ()
        block
      case (NEWLINE, _) :: _ => consume; block
      case _ => WithContext(Nil, context)

  /** Parse a keyword declaration */
  def parseKeyword(using context: Context): Context =
    /** Parse a keyword and its right precedence */
    def kwr: Option[(String, Option[Int])] = yeetSpaces match
      case (id: IDENT, _) :: _ => consume;
        yeetSpaces match
        case (tok @ LITVAL(Literal.IntLit(rightPrec)), loc) :: _ =>
          consume; Some((id.name, Some(rightPrec.toInt)))
        case _ => Some((id.name, None))
      case (tok, loc) :: _ => unexpected("an identifier", tok.describe, S(loc), None)
      case Nil => unexpected("an identifier", "end of input", lastLoc, None)
    /** Parse a keyword starting with an optional left precedence */
    def kw: Option[Keyword] = yeetSpaces match
      case (LITVAL(Literal.IntLit(leftPrec)), _) :: _ => consume; kwr.map:
        case(name, rightPrec) => Keyword(name, Some(leftPrec.toInt), rightPrec)
      case (_: IDENT, _) :: _ => kwr.map:
        case(name, rightPrec) => Keyword(name, None, rightPrec)
      case (tok, loc) :: _ => unexpected("an identifier", tok.describe, S(loc), None)
      case Nil => unexpected("an identifier", "end of input", lastLoc, None)
    @tailrec def kwCont(acc: List[Keyword]): List[Keyword] = yeetSpaces match
      case (COMMA, _) :: _ => consume; kw match
        case Some(k) => kwCont(k :: acc)
        case None => acc.reverse
      case _ => acc.reverse
    kw.fold(context)(k => context ++ kwCont(k :: Nil))

  /** Parse a define declaration */
  def parseDefine(using context: Context): Context =
    def parseRhs: Ls[Tree] => Tree =
      val tree = expr(0)
      (args) => subst(tree, args.toArray)
    def badEnd(found: String, loco: Option[Loc]): Alt[Ls[Tree]] =
      unexpected("keywords, `Ident`, `Expr`, or other non-terminal symbol in rule definitions", found, loco, Alt.End(Nil: List[Tree]))
    def symbol(id: IDENT, loc: Loc): Alt[Ls[Tree]] = id.name match
      case "Ident" => Alt.Ident(ParseRule(s"after Ident")(parseLhs))(_ :: _)
      case "Expr" => Alt.Expr(ParseRule(s"after Expr")(parseLhs))(_ :: _)
      case _ => badEnd(id.describe, Some(loc))
    def parseLhs: Alt[Ls[Tree]] = wrap("define lhs"):
      yeetSpaces match
      case (tok @ KW(KW.`~>`), loc) :: _ => consume; Alt.End(Nil)
      case (tok @ KW(kw), loc) :: _ =>
        consume; Alt.Kw(kw)(ParseRule(s"after ${kw.name}")(parseLhs))
      case (id: IDENT, loc) :: _ => consume; symbol(id, loc)
      case (tok, loc) :: _ => badEnd(tok.describe, Some(loc))
      case Nil => badEnd("end of input", lastLoc)
    expectIdent("`define`", context): (id, loc) =>
      expectKeyword(KW.`::=`, "the name", context): (tok, loc) =>
        context + (id.name -> parseLhs.map(parseRhs))

  private def tryKwAlt[A](id: IDENT, loc: Loc, rule: ParseRule[A], failure: => A)(otherwise: => A)(using context: Context): A =
    wrap(""):
      context.keywords.get(id.name) match
      case S(kw) =>
        rule.kwAlts.get(kw.name) match
          case S(subRule) =>
            consume
            parseRule(CommaPrecNext, subRule).getOrElse(failure)
          case N => otherwise
      case N => otherwise

  private def tryIdentAlt[A](id: IDENT, loc: Loc, rule: ParseRule[A], failure: => A)(otherwise: => A)(using Context): A =
    wrap(""):
      rule.identAlt match
      case S(identAlt) =>
        consume
        parseRule(CommaPrecNext, identAlt.rest)
          .map(res => identAlt.k(Tree.Var(id.name), res))
          .getOrElse(failure)
      case N => otherwise

  private def tryExprAlt[A](id: IDENT, loc: Loc, rule: ParseRule[A], failure: => A)
      (otherwise: => A)(using Context): A =
    wrap(""):
      rule.exprAlt match
      case S(exprAlt) =>
        val e = expr(0) // which prec?
        parseRule(CommaPrecNext, exprAlt.rest).map(res => exprAlt.k(e, res)).getOrElse(failure)
      case N => otherwise
  
  /** If the rule accepts an expression, continue subsequent rules. Otherwise, try the empty.  */
  private def tryExprThenEmpty[A](prec: Int, tok: Token, loc: Loc, rule: ParseRule[A])(using Context): Option[A] =
    wrap(s"$prec, ${tok.describe}"):
      rule.exprAlt match
      case S(exprAlt) =>
        val e = expr(prec)
        parseRule(prec, exprAlt.rest).map(res => exprAlt.k(e, res))
      case N => tryEmpty(rule, tok.describe, loc)

  /** If the rule accepts an empty, return the result, otherwise report the unexpectedness.  */
  private def tryEmpty[A](rule: ParseRule[A], toks: String, loc: Loc): Option[A] = rule.emptyAlt match
    case S(outcome) => S(outcome)
    case N => consume; unexpected(s"${rule.whatComesAfter} after ${rule.name}", toks, S(loc), N)
  
  /** A result of None means there was an error (already reported) and nothing could be parsed. */
  def parseRule[A](prec: Int, rule: ParseRule[A])(using context: Context): Opt[A] = wrap(prec, rule):
    yeetSpaces match
    case (tok @ (id: IDENT), loc) :: _ =>
      context.keywords.get(id.name) match
      case S(kw) =>
        consume
        rule.kwAlts.get(id.name) match
        case S(subRule) =>
          consume
          parseRule(kw.rightPrec.getOrElse(0), subRule)
        case N =>
          // No rules begin with an identifer. Try `exprAlt`.
          rule.exprAlt match
          case S(exprAlt) =>
            consume
            context.prefixRules.kwAlts.get(id.name) match
            case S(subRule) =>
              // parse(subRule)
              val e = parseRule(kw.rightPrec.getOrElse(0), subRule).getOrElse(errExpr)
              parseRule(prec, exprAlt.rest).map(res => exprAlt.k(e, res))
            case N => tryEmpty(rule, tok.describe, loc)
          case N => tryEmpty(rule, tok.describe, loc)
      case N =>
        // Not a keyword. Try `identAlt`.
        printDbg(s"try indent alt for ${id.name}")
        rule.identAlt match
        case S(identAlt) =>
          consume
          parseRule(prec, identAlt.rest).map(res => identAlt.k(Tree.Var(id.name), res))
        case N => tryExprThenEmpty(prec, tok, loc, rule)
    case (tok @ (SEMI | COMMA), loc) :: _ => tryEmpty(rule, tok.describe, loc)
    case (tok, loc) :: _ => tryExprThenEmpty(prec, tok, loc, rule)
    case Nil => tryEmpty(rule, "end of input", lastLoc.get)
  
  def expr(prec: Int)(using Line, Context): Tree = wrap(prec):
    yeetSpaces match
    case (IDENT(nme, sym), loc) :: _ =>
      consume
      exprCont(Tree.Var(nme).withLoc(loc), prec, allowNewlines = true)
    case (LITVAL(lit), loc) :: _ =>
      consume
      exprCont(lit.asTree, prec, allowNewlines = true)
    case (OPEN_BRACKET(Round), loc) :: _ =>
      printDbg("found an opening bracket")
      consume
      val res = yeetSpaces match
        case (CLOSE_BRACKET(Round), loc) :: _ =>
          consume
          Literal.UnitLit.asTree.withLoc(loc)
        case _ =>
          closeBracket(expr(0))
      exprCont(res, prec, allowNewlines = true)
    case (tok, loc) :: _ => unexpected("an expression", tok, loc)
    case Nil => unexpected("an expression")
  
  import Tree.*
  
  final def exprCont(acc: Tree, prec: Int, allowNewlines: Bool)(using Context): Tree = wrap(prec, s"`$acc`", allowNewlines):
    cur match
      case (COMMA, l0) :: _ if prec === 0 =>
        consume
        Apps(Var(",").withLoc(S(l0)), acc, expr(prec))
      case (KW(kw @ KW.`=>`), l0) :: _ if kw.leftPrecOrMin > prec =>
        consume
        val rhs = expr(kw.rightPrecOrMin)
        val res = Lam(acc, rhs)
        exprCont(res, prec, allowNewlines)
      case (OP(opStr), l0) :: _ if /* isInfix(opStr) && */ opPrec(opStr)._1 > prec =>
        consume
        val v = Var(opStr).withLoc(S(l0))
        val rhs = expr(opPrec(opStr)._2)
        exprCont(Apps(v, acc, rhs), prec, allowNewlines)
      case (SPACE, l0) :: _ =>
        consume
        exprCont(acc, prec, allowNewlines)
      case (SELECT(name), l0) :: _ => // TODO precedence?
        consume
        exprCont(Apps(Var("."), acc, Var(name).withLoc(S(l0))), prec, allowNewlines)
      case (br @ OPEN_BRACKET(Round), loc) :: _ if prec <= AppPrec =>
        consume
        closeBracket(exprCont(App(acc, expr(0)), prec, allowNewlines))
      case (CLOSE_BRACKET(Round), loc) :: _ =>
        printDbg("found a closing bracket")
        acc
      // case (KW(kw), l0) :: _ if kw.leftPrecOrMin > prec =>
      //   ParseRule.infixRules.kwAlts.get(kw.name) match
      //     case S(rule) =>
      //       consume
      //       rule.exprAlt match
      //         case S(exprAlt) =>
      //           yeetSpaces match
      //             case (NEWLINE, l0) :: _ =>
      //               consume
      //               ???
      //             case _ =>
      //           val rhs = expr(kw.rightPrecOrMin)
      //           parseRule(kw.rightPrecOrMin, exprAlt.rest).map: rest =>
      //             exprCont(exprAlt.k(rhs, rest)(acc), prec, allowNewlines) // FIXME prec??
      //           .getOrElse(errExpr)
      //         case N =>
      //           // TODO other alts...?
      //           err((msg"Expected ${rule.whatComesAfter} after ${rule.name}; found ${kw.name} instead" -> S(l0) :: Nil))
      //           acc
      //     case _ => acc
      case _ => acc

  private def closeBracket(e: Tree)(using Context): Tree =
    yeetSpaces match
    case (CLOSE_BRACKET(Round), loc) :: _ => consume; e
    case (tok, loc) :: _ => unexpected("a closing parenthesis", tok.describe, S(loc), e)
    case Nil => unexpected("a closing parenthesis", "end of input", lastLoc, e)

  /** A brute-force substitution disregarding any existing abstractions */
  private def subst(t: Tree, args: Array[Tree]): Tree = t match
    case t @ Tree.Var(nme) if nme.startsWith("$") =>
      nme.tail.toIntOption match
      case Some(n) if 1 <= n && n <= args.length => args(n - 1)
      case _ => warn(msg"Invalid meta-variable reference ${nme}" -> t.toLoc :: Nil); t
    case Tree.App(f, a) => Tree.App(subst(f, args), subst(a, args))
    case Tree.Lam(lhs, rhs) => Tree.Lam(subst(lhs, args), subst(rhs, args))
    case _ => t

end Parser
