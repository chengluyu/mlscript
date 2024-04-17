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
    val `syntax` = Keyword("syntax", None, None)
    val `=>` = Keyword("=>", Some(BASE + 1), Some(BASE))

    def unapply(t: Token)(using context: Context): Option[Keyword] = t match
      case id: IDENT if !id.escaped => context.keyword(id)
      case _ => None
  
  object OP:
    def unapply(t: Token)(using context: Context): Option[String] = t match
      case IDENT(name, true) if !context.keywords.contains(name) => Some(name)
      case _ => None

  object PREC:
    def unapply(t: Token): Option[Int] = t match
      case LITVAL(Literal.IntLit(prec)) => Some(prec.toInt)
      case IDENT("max", _) => Some(Int.MaxValue)
      case IDENT("min", _) => Some(Int.MinValue)
      case _ => None

  private val captureNamePattern = """\$([a-zA-Z_][a-zA-Z0-9_]*|\d+)""".r

  def interpolate(text: String, args: Array[Tree], nameMap: Map[String, Int]): String =
    captureNamePattern.replaceAllIn(text, _.group(1) match
      case n => n.toIntOption match
        case Some(i) => args(i - 1).print
        case None => args(nameMap(n)).print)
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
    printDbg(s"= ${res.toStableString}")
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

  def unexpectedEOI[A](expected: String, res: A): A =
    unexpected(expected, "end of input", lastLoc, res)

  def unexpectedEOI(expected: String): Tree = unexpectedEOI(expected, errExpr)
  
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
  
  final def block(using context: Context): WithContext[List[Tree]] = wrap("block"):
    inline def blockCont(using context: Context): WithContext[List[Tree]] =
      yeetSpaces match
      case (COMMA | SEMI, _) :: _ =>
        consume
        yeetSpaces match
          case (NEWLINE, _) :: _ => consume
          case _ => ()
        block
      case (NEWLINE, _) :: _ => consume; block
      case _ => WithContext(Nil, context)
    yeetSpaces match
    case Nil => WithContext(Nil, context)
    case (tok @ KW(KW.`keyword`), loc) :: _ =>
      consume; blockCont(using parseKeyword)
    case (tok @ KW(KW.`define`), loc) :: _ =>
      consume
      expectIdent(previous = "define", blockCont): (id, loc) =>
        expectKeyword(KW.`::=`, previous = "rule name", blockCont): (tok, loc) =>
          blockCont(using context + (id.name -> parseDefine))
    case (tok @ KW(KW.`syntax`), loc) :: _ =>
      consume; blockCont(using context + parseDefine)
    case (tok, loc) :: _ => // Otherwise, parse expressions.
      // Note: leading keywords will be handled in `expr`.
      // Note: we don't try `IdentAlt` in the beginning.
      val hd = expr(0)
      blockCont.map(hd :: _)

  def parsePrecedence(isLeft: Bool)(using Context): Option[Int] = wrap(""):
    yeetSpaces match
    case (LITVAL(Literal.IntLit(prec)), _) :: _ => consume; Some(prec.toInt)
    case (OPEN_BRACKET(Round), _) :: _ =>
      consume
      yeetSpaces match
      case (PREC(prec), _) :: _ => consume; closeBracket(Some(prec), None)
        case Nil => unexpectedEOI("a closing parenthesis", None)
      // (stronger|weaker) than op
      case (IDENT(rel @ ("stronger" | "weaker"), _), _) :: _ => consume; yeetSpaces match
        case (IDENT("than", _), _) :: _ => consume; yeetSpaces match
          case (IDENT(op, true), _) :: _ => consume; opPrec(op) match
            case (left, right) => closeBracket(Some(if isLeft then left else right)) // TODO: decimal
          case (tok, loc) :: _ => unexpected("an operator", tok.describe, S(loc), None)
          case Nil => unexpectedEOI("an operator", None)
        case (tok, loc) :: _ => unexpected("than", tok.describe, S(loc), None)
        case Nil => unexpectedEOI("than", None)
      case (tok, loc) :: _ => unexpected("an integer, min, max or precedence relations", tok.describe, S(loc), None)
      // case Nil => unexpectedEOI("an integer, min, max or precedence relations", None) // TODO: why unreachable
    case _ :: _ | Nil => None

  /** Parse a keyword declaration */
  def parseKeyword(using context: Context): Context =
    /** Parse a keyword and its right precedence */
    def kwr: Option[(String, Option[Int])] = yeetSpaces match
      case (id: IDENT, _) :: _ => consume; Some((id.name, parsePrecedence(isLeft = false)))
      case (tok, loc) :: _ => unexpected("an identifier", tok.describe, S(loc), None)
      case Nil => unexpected("an identifier", "end of input", lastLoc, None)
    /** Parse a keyword starting with an optional left precedence */
    def kw: Option[Keyword] =
      val leftPrec = parsePrecedence(isLeft = true)
      yeetSpaces match
      case (_: IDENT, _) :: _ => kwr.map:
        case (name, rightPrec) => Keyword(name, leftPrec, rightPrec)
      case (tok, loc) :: _ => unexpected("an identifier", tok.describe, S(loc), None)
      case Nil => unexpected("an identifier", "end of input", lastLoc, None)
    @tailrec def kwCont(acc: List[Keyword]): List[Keyword] = yeetSpaces match
      case (COMMA, _) :: _ => consume; kw match
        case Some(k) => kwCont(k :: acc)
        case None => acc.reverse
      case _ => acc.reverse
    kw.fold(context)(k => context ++ kwCont(k :: Nil))

  def badDef[A](acc: A, expected: String, actual: String, loco: Option[Loc]): (Alt[Ls[Tree]], A) =
    unexpected(expected, actual, loco, (Alt.End(Nil: List[Tree]), acc))
  def badDef[A](acc: A, actual: String, loco: Option[Loc]): (Alt[Ls[Tree]], A) =
    badDef(acc, "keywords, `Ident`, `Expr`, or other non-terminal symbol in rule definitions", actual, loco)

  /** Parse a define declaration */
  def parseDefine(using context: Context): Alt[Tree] =
    def parseRhs(nameMap: Map[String, Int]): Ls[Tree] => Tree =
      val tree = expr(0)
      (args) => subst(tree, args.toArray, nameMap)
    def parseLhs(acc: List[Option[String]]): (Alt[Ls[Tree]], List[Option[String]]) = wrap("define lhs"):
      def symbol(acc: List[Option[String]], name: Option[String], symbol: IDENT, loc: Loc) = symbol.name match
        case "Ident" =>
          val (alt, names) = parseLhs(name :: acc)
          (Alt.Ident(ParseRule(s"after Ident")(alt))(_ :: _), names)
        case "Expr" | "_" =>
          val (alt, names) = parseLhs(name :: acc)
          (Alt.Expr(ParseRule(s"after Expr")(alt))(_ :: _), names)
        case _  => badDef(acc, symbol.describe, Some(loc))
      yeetSpaces match
      case (NEWLINE, _) :: _ => consume; parseLhs(acc)
      case (tok @ KW(KW.`~>`), loc) :: _ =>
        consume
        yeetSpaces match
          case (NEWLINE, _) :: _ => consume
          case _ => ()
        (Alt.End(Nil), acc.reverse)
      case (tok @ KW(kw), loc) :: _ =>
        consume
        val (alt, names) = parseLhs(acc)
        (Alt.Kw(kw)(ParseRule(s"after ${kw.name}")(alt)), names)
      case (id: IDENT, loc) :: _ => consume; symbol(acc, None, id, loc)
      case (OPEN_BRACKET(Angle), _) :: _ => consume; yeetSpaces match // <
        case (id1: IDENT, loc) :: _ => consume; yeetSpaces match // ident
          case (IDENT(":", _), _) :: _ => consume; yeetSpaces match // :
            case (id2: IDENT, loc) :: _ => consume; yeetSpaces match // ident
              case (CLOSE_BRACKET(Angle), _) :: _ => // >
                consume; symbol(acc, Some(id1.name), id2, loc)
              case (tok, loc) :: _ => badDef(acc, "right angle bracket", tok.describe, Some(loc))
              case Nil => badDef(acc, "right angle bracket", "end of input", lastLoc)
            case (tok, loc) :: _ => badDef(acc, tok.describe, Some(loc))
            case Nil => badDef(acc, "end of input", lastLoc)
          case (CLOSE_BRACKET(Angle), _) :: _ => consume; symbol(acc, None, id1, loc) // >
          case (tok, loc) :: _ => badDef(acc, ": or >", tok.describe, Some(loc))
          case Nil => badDef(acc, ": or >", "end of input", lastLoc)
        case (tok, loc) :: _ => badDef(acc, "an identifier", tok.describe, Some(loc))
        case Nil => badDef(acc, "an identifier", "end of input", lastLoc)
      case (tok, loc) :: _ => badDef(acc, tok.describe, Some(loc))
      case Nil => badDef(acc, "end of input", lastLoc)
    val (alt, names) = parseLhs(Nil)
    val namePairs = names.iterator.zipWithIndex.collect:
      case (Some(n), i) => n -> i
    alt.map(parseRhs(namePairs.toMap))
  
  /** If the rule accepts an expression, continue subsequent rules. Otherwise, try the empty.  */
  private def tryExprThenEmpty[A](prec: Int, tok: Token, loc: Loc, rule: ParseRule[A], afterExpr: Bool)(using Context): Option[A] =
    wrap(s"$prec, ${tok.describe}"):
      rule.exprAlts match
      // Allow an optional NEWLINE if the following is also an expression.
      case exprAlts: ::[Alt.Expr[?, A]] =>
        if afterExpr then yeetSpaces match
          case (NEWLINE, _) :: _ => consume;
          case _ => ()
        val e = expr(prec)
        // Try each `exprAlt` by peeking its `rest`.
        // This can be improved by pre-processing rules.
        // It is mostly used in parsing optional symbols.
        // Only `Kw` and `End` are implemented.
        yeetSpaces match
          case (KW(kw), _) :: _ =>
            // Find the first `exprAlt` that accepts the keyword.
            exprAlts.iterator.flatMap:
              case alt: Alt.Expr[rst, A] =>
                alt.rest.kwRules.get(kw).map:
                  case rst => rst.map(res => alt.k(e, res))
                  //   ^^^ `rst` is the rule after `kw`
            .nextOption.flatMap:
              case rule => parseRule(kw.rightPrecOrMin, rule, false)
          case _ =>
            tryEmpty(rule, tok.describe, loc)
        // rule.exprAlts.iterator.map(_.rest).map(parseRule(prec, _, true)).collectFirst:
        //   case Some(res) => res.map(rule.exprAlts.head.k(e, _))
        //   case None => tryEmpty(rule, tok.describe, loc)
        // parseRule(prec, exprAlt.rest, true).map(res => exprAlt.k(e, res))
      case Nil => tryEmpty(rule, tok.describe, loc)

  /** If the rule accepts an empty, return the result, otherwise report the unexpectedness.  */
  private def tryEmpty[A](rule: ParseRule[A], toks: String, loc: Loc): Option[A] = rule.emptyAlt match
    case S(outcome) => S(outcome)
    case N => consume; unexpected(s"${rule.whatComesAfter} after ${rule.name}", toks, S(loc), N)
  
  /** A result of None means there was an error (already reported) and nothing could be parsed. */
  def parseRule[A](prec: Int, rule: ParseRule[A], afterExpr: Bool)(using context: Context): Opt[A] = wrap(prec, rule):
    yeetSpaces match
    case (tok @ (id: IDENT), loc) :: _ =>
      context.keyword(id) match
      // case S(kw) if kw.leftPrec.exists(_ >= prec) =>
      // Note: we only check precedence when picking new rules.
      // If there is an existing rule, we don't need to check precedence.
      case S(kw) =>
        rule.kwRules.get(kw) match
        case S(subRule) =>
          consume
          parseRule(kw.rightPrecOrMin, subRule, false)
        case N => tryExprThenEmpty(prec, tok, loc, rule, afterExpr)
      case _ =>
        rule.identAlt match
        case S(identAlt) =>
          consume
          parseRule(prec, identAlt.rest, false).map(res => identAlt.k(Tree.Var(id.name), res))
        case N => tryExprThenEmpty(prec, tok, loc, rule, afterExpr)
    case (tok @ (SEMI | COMMA), loc) :: _ => tryEmpty(rule, tok.describe, loc)
    case (tok, loc) :: _ => tryExprThenEmpty(prec, tok, loc, rule, afterExpr)
    case Nil => tryEmpty(rule, "end of input", lastLoc.get)
  
  def expr(prec: Int)(using context: Context): Tree = wrap(prec):
    yeetSpaces match
    case (KW(kw), loc) :: _ => consume; context.prefix(kw) match
      case S(rule) =>
        if kw.leftPrec.exists(_ >= prec)
        then parseRule(kw.rightPrec.getOrElse(0), rule, false)
          .map(lhs => exprCont(lhs, prec, allowNewlines = true))
          .getOrElse(errExpr)
        else unexpected("an expression", kw.toString, S(loc))
      case N => unexpected("an expression", kw.toString, S(loc))
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
        case _ => closeBracket(expr(0))
      exprCont(res, prec, allowNewlines = true)
    case (tok, loc) :: _ => unexpected("an expression", tok, loc)
    case Nil => unexpectedEOI("an expression")
  
  import Tree.*
  
  final def exprCont(acc: Tree, prec: Int, allowNewlines: Bool)(using context: Context): Tree = wrap(prec, s"`$acc`", allowNewlines):
    cur match
      case (COMMA, l0) :: _ if prec === 0 =>
        consume
        Apps(Var(",").withLoc(S(l0)), acc, expr(prec))
      case (KW(kw @ KW.`=>`), l0) :: _ if kw.leftPrecOrMin > prec =>
        consume
        val rhs = expr(kw.rightPrecOrMin)
        val res = Lam(acc, rhs)
        exprCont(res, prec, allowNewlines)
      case (KW(kw), loc) :: _ if kw.leftPrecOrMin > prec => // TODO: Using `leftPrecOrMin` or `leftPrecOrMax`?
        printDbg(s"looking for infix rules for $kw")
        consume
        context.infixRules.get(kw) match
          case Some(rule) => // Suppose we're in `Expr KW rule`
            parseRule(kw.rightPrecOrMin, rule, true).map(_(acc)).getOrElse(errExpr)
          case None =>
            val legalKws = context.infixRules.keys.iterator
              .filter(_.leftPrecOrMin > prec).map(_.name).mkString(", ")
            unexpected(s"an infix operator $legalKws", kw.toString, S(loc), acc)
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
        val res = closeBracket(exprCont(App(acc, expr(0)), prec, allowNewlines))
        exprCont(res, prec, allowNewlines)
      case _ => acc

  private def closeBracket[A](res: A)(using Context): A = closeBracket(res, res)

  private def closeBracket[A](res: A, failure: => A)(using Context): A =
    yeetSpaces match
    case (CLOSE_BRACKET(Round), loc) :: _ => consume; res
    case (tok, loc) :: _ => unexpected("a closing parenthesis", tok.describe, S(loc), failure)
    case Nil => unexpected("a closing parenthesis", "end of input", lastLoc, failure)

  private def subst(t: Tree, args: Array[Tree], nm: Map[String, Int]): Tree = t match
    case t @ Tree.Var(nme) if nme.startsWith("$") =>
      nme.tail.toIntOption match
      case Some(n) => if 1 <= n && n <= args.length then args(n - 1) else
        warn(msg"Invalid meta-variable index ${n.toString}" -> t.toLoc :: Nil); t
      case None => nm.get(nme.tail) match
        case Some(n) => assert(0 <= n && n < args.length); args(n)
        case None => warn(msg"Invalid meta-variable name ${nme}" -> t.toLoc :: Nil); t
    case Tree.App(f, a) => Tree.App(subst(f, args, nm), subst(a, args, nm))
    case Tree.Lam(lhs, rhs) => Tree.Lam(subst(lhs, args, nm), subst(rhs, args, nm))
    case Tree.Const(Literal.StrLit(text)) =>
      Tree.Const(Literal.StrLit(interpolate(text, args, nm)))
    case _ => t

end Parser
