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
  
  type TokLoc = (Stroken, Loc)
  
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
end Parser
import Parser._

abstract class Parser(
  origin: Origin,
  tokens: Ls[TokLoc],
  raiseFun: Diagnostic => Unit,
  val dbg: Bool,
  // fallbackLoc: Opt[Loc], description: Str = "input",
)(using context: Context):
  outer =>
  
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
  
  final def rec(tokens: Ls[Stroken -> Loc], fallbackLoc: Opt[Loc], description: Str): Parser =
    new Parser(origin, tokens, raiseFun, dbg
        // , fallbackLoc, description
    ):
      def doPrintDbg(msg: => Str): Unit = outer.printDbg("> " + msg)
  
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

  def unexpected(expected: String, tok: Stroken, loc: Loc): Tree =
    unexpected(expected, tok.describe, S(loc))

  def unexpected(expected: String, actual: String, loc: Option[Loc]): Tree =
    unexpected(expected, actual, loc, errExpr)

  def unexpected(expected: String): Tree = unexpected(expected, "end of input", lastLoc)
  
  final def parseAll[R](parser: => R): R =
    val res = parser
    cur match
      case c @ (tk, tkl) :: _ =>
        val (relevantToken, rl) = c.dropWhile(_._1 === SPACE).headOption.getOrElse(tk, tkl)
        err(msg"Expected end of input; found ${relevantToken.describe} instead" -> S(rl) :: Nil)
      case Nil => ()
    res
  
  final def concludeWith[R](f: this.type => R): R =
    val res = f(this)
    cur.dropWhile(tk => (tk._1 === SPACE) && { consume; true }) match
      case c @ (tk, tkl) :: _ =>
        val (relevantToken, rl) = c.dropWhile(_._1 === SPACE).headOption.getOrElse(tk, tkl)
        err(msg"Unexpected ${relevantToken.describe} here" -> S(rl) :: Nil)
      case Nil => ()
    printDbg(s"Concluded with $res")
    res
  
  def block: Ls[Tree] = blockOf(context.prefixRules)

  /** Expect the next token is an identifier */
  def expectIdent[A](previous: String, failure: => A)(success: (IDENT, Loc) => A): A =
    yeetSpaces match
    case (tok @ (id: IDENT), loc) :: _ => consume; success(id, loc)
    case (tok, loc) :: _ => unexpected(s"an identifier after $previous", tok.describe, S(loc), failure)
    case Nil => unexpected(s"an identifier after $previous", "end of input", lastLoc, failure)

  /** Expect the next token is the given keyword */
  def expectKeyword[A](keyword: Keyword, previous: String, failure: => A)(success: (Stroken, Loc) => A): A =
    yeetSpaces match
    case (tok @ context.KW(keyword), loc) :: _ => consume; success(tok, loc)
    case (tok, loc) :: _ => unexpected(s"${keyword.name} after $previous", tok.describe, S(loc), failure)
    case Nil => unexpected(s"${keyword.name} after $previous", "end of input", lastLoc, failure)
  
  def blockOf(rule: ParseRule[Tree]): Ls[Tree] = wrap(rule.name):
    cur match
    case Nil => Nil
    case (SPACE, _) :: _ => consume; blockOf(rule)
    case (tok @ context.KW(context.`keyword`), loc) :: _ =>
      consume; parseKeyword; blockContOf(context.prefixRules)
    case (tok @ context.KW(context.`define`), loc) :: _ =>
      consume; parseDefine; blockContOf(context.prefixRules)
    case (tok @ (id: IDENT), loc) :: _ =>
      parseIdent(id, loc, rule, errExpr) :: blockContOf(context.prefixRules)
    case (tok, loc) :: _ =>
      tryParseExp(CommaPrecNext, tok, loc, rule).getOrElse(errExpr) :: blockContOf(context.prefixRules)
  
  def blockContOf(rule: ParseRule[Tree]): Ls[Tree] =
    yeetSpaces match
      case (COMMA | SEMI, _) :: _ =>
        consume
        yeetSpaces match
          case (NEWLINE, _) :: _ => consume
          case _ => ()
        blockOf(rule)
      case (NEWLINE, _) :: _ => consume; blockOf(rule)
      case _ => Nil

  /** Parse a keyword declaration */
  def parseKeyword: Unit =
    def kw: Unit = yeetSpaces match
      case (id: IDENT, _) :: _ =>
        consume
        yeetSpaces match // the left precedence
        case (tok @ LITVAL(Literal.IntLit(leftPrec)), loc) :: _ =>
          consume
          yeetSpaces match // the right precedence
          case (tok @ LITVAL(Literal.IntLit(rightPrec)), loc) :: _ =>
            consume; context += Keyword(id.name, Some(leftPrec.toInt), Some(rightPrec.toInt))
          case _ => context += Keyword(id.name, Some(leftPrec.toInt), None)
        case _ => context += Keyword(id.name, None, None)
      case (tok, loc) :: _ => unexpected("an identifier", tok, loc)
      case Nil => unexpected("an identifier")
    def kwCont: Unit = yeetSpaces match
      case (COMMA, _) :: _ => consume; kw
      case _ => ()
    kw
    kwCont

  /** Parse a define declaration */
  def parseDefine: Unit =
    def parseRhs: Ls[Tree] => Tree =
      val tree = expr(0)
      (args) => subst(tree, args.toArray)
    def badEnd(found: String, loco: Option[Loc]): Alt[Ls[Tree]] =
      unexpected("`Ident`, `Expr`, or other non-terminal symbol in rule definitions", found, loco, Alt.End(Nil: List[Tree]))
    def symbol(id: IDENT, loc: Loc): Alt[Ls[Tree]] =
      context.getKeyword(id.name) match
        case S(kw) => Alt.Kw(kw)(ParseRule(s"after ${kw.name}")(parseLhs))
        case N => id.name match
          case "Ident" => Alt.Ident(ParseRule(s"after Ident")(parseLhs))(_ :: _)
          case "Expr" => Alt.Expr(ParseRule(s"after Expr")(parseLhs))(_ :: _)
          case _ => badEnd(id.describe, Some(loc))
    def parseLhs: Alt[Ls[Tree]] = wrap("define lhs"):
      yeetSpaces match
      case (tok @ context.KW(context.`~>`), loc) :: _ => consume; Alt.End(Nil)
      case (tok @ context.RULE(rule), loc) :: _ =>
        consume
        Alt.Ref(rule.name, rule)(ParseRule(s"after Ref")(parseLhs))(_ :: _)
      case (id: IDENT, loc) :: _ => consume; symbol(id, loc)
      case (tok, loc) :: _ => badEnd(tok.describe, Some(loc))
      case Nil => badEnd("end of input", lastLoc)
    expectIdent("`define`", ()): (id, loc) =>
      expectKeyword(context.`::=`, "the name", ()): (tok, loc) =>
        context += id.name -> parseLhs.map(parseRhs)

  private def tryKwAlt[A](id: IDENT, loc: Loc, rule: ParseRule[A], failure: => A)(otherwise: => A): A =
    context.getKeyword(id.name) match
      case S(kw) =>
        rule.kwAlts.get(kw.name) match
          case S(subRule) =>
            consume
            parseRule(CommaPrecNext, subRule).getOrElse(failure)
          case N => otherwise
      case N =>
        otherwise

  private def tryIdentAlt[A](id: IDENT, loc: Loc, rule: ParseRule[A], failure: => A)(otherwise: => A): A =
    rule.identAlt match
      case S(identAlt) =>
        consume
        parseRule(CommaPrecNext, identAlt.rest)
          .map(res => identAlt.k(Tree.Var(id.name), res))
          .getOrElse(failure)
      case N =>
        otherwise

  private def tryExprAlt[A](id: IDENT, loc: Loc, rule: ParseRule[A], failure: => A)(otherwise: => A): A =
    // No rules begin with an identifer. Try `exprAlt`.
    rule.exprAlt match
      case S(exprAlt) =>
        val e = expr(0) // which prec?
        parseRule(CommaPrecNext, exprAlt.rest).map(res => exprAlt.k(e, res)).getOrElse(failure)
      case N => otherwise
  
  private def tryParseExp[A](prec: Int, tok: Token, loc: Loc, rule: ParseRule[A]): Opt[A] =
    rule.exprAlt match
      case S(exprAlt) =>
        val e = expr(prec)
        parseRule(prec, exprAlt.rest).map(res => exprAlt.k(e, res))
      case N => tryEmpty(rule, tok.describe, loc)

  private def tryEmpty[A](rule: ParseRule[A], found: String, loc: Loc) = rule.emptyAlt match
    case S(res) => S(res)
    case N =>
      consume
      err((msg"Expected ${rule.whatComesAfter} after ${rule.name}; found $found instead" -> S(loc) :: Nil))
      N

  def parseIdent[A](id: IDENT, loc: Loc, rule: ParseRule[A], failure: => A): A = wrap("parseIdent"):
    tryKwAlt(id, loc, rule, failure):
      tryIdentAlt(id, loc, rule, failure):
        tryExprAlt(id, loc, rule, failure):
          unexpected(s"${rule.whatComesAfter} after ${rule.name}", id.describe, S(loc), failure)
  
  /** A result of None means there was an error (already reported) and nothing could be parsed. */
  def parseRule[A](prec: Int, rule: ParseRule[A]): Opt[A] = wrap(prec, rule):
    yeetSpaces match
    case (tok @ (id: IDENT), loc) :: _ =>
      context.getKeyword(id.name) match
      case S(kw) =>
        consume
        rule.kwAlts.get(id.name) match
        case S(subRule) =>
          consume
          parseRule(kw.rightPrec.getOrElse(0), subRule)
        case N =>
          // No rules begin with this keyword. Try `identAlt`.
          rule.identAlt match
          case S(identAlt) =>
            consume
            parseRule(prec, identAlt.rest).map(res => identAlt.k(Tree.Var(id.name), res))
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
              case N =>
                tryEmpty(rule, tok.describe, loc)
            case N =>
              tryEmpty(rule, tok.describe, loc)
      case N =>
        // Not a keyword. Try `identAlt`.
        printDbg(s"try indent alt for ${id.name}")
        rule.identAlt match
        case S(identAlt) =>
          consume
          parseRule(prec, identAlt.rest).map(res => identAlt.k(Tree.Var(id.name), res))
        case N =>
          tryParseExp(prec, tok, loc, rule)
    case (tok @ (SEMI | COMMA), loc) :: _ => tryEmpty(rule, tok.describe, loc)
    case (tok, loc) :: _ => tryParseExp(prec, tok, loc, rule)
    case Nil => tryEmpty(rule, "end of input", lastLoc.get)
  
  def expr(prec: Int)(using Line): Tree = wrap(prec):
    yeetSpaces match
    case (IDENT(nme, sym), loc) :: _ =>
      consume
      exprCont(Tree.Var(nme).withLoc(loc), prec, allowNewlines = true)
    case (LITVAL(lit), loc) :: _ =>
      consume
      exprCont(lit.asTree, prec, allowNewlines = true)
    case (BRACKETS(Round, toks), loc) :: _ =>
      consume
      if toks.forall(_ is SPACE) then
        val res = Literal.UnitLit.asTree.withLoc(loc)
        exprCont(res, prec, allowNewlines = true)
      else
        val res = rec(toks, S(loc), "parenthesized expression").concludeWith(_.expr(0))
        exprCont(res, prec, allowNewlines = true)
    case (tok, loc) :: _ => unexpected("an expression", tok, loc)
    case Nil => unexpected("an expression")
  
  import Tree.*
  
  final def exprCont(acc: Tree, prec: Int, allowNewlines: Bool): Tree = wrap(prec, s"`$acc`", allowNewlines):
    cur match
      case (COMMA, l0) :: _ if prec === 0 =>
        consume
        Apps(Var(",").withLoc(S(l0)), acc, expr(prec))
      case (context.KW(kw @ context.`=>`), l0) :: _ if kw.leftPrecOrMin > prec =>
        consume
        val rhs = expr(kw.rightPrecOrMin)
        val res = Lam(acc, rhs)
        exprCont(res, prec, allowNewlines)
      case (context.OP(opStr), l0) :: _ if /* isInfix(opStr) && */ opPrec(opStr)._1 > prec =>
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
      case (br @ BRACKETS(Round, toks), loc) :: _ if prec <= AppPrec =>
        consume
        // val as = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.blockMaybeIndented)
        val as = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.expr(0))
        val res = Apps(acc, as)
        exprCont(res, prec, allowNewlines)
      // case (context.KW(kw), l0) :: _ if kw.leftPrecOrMin > prec =>
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
