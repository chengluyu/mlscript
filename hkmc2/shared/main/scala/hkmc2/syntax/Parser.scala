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

  def expectIdent[A](previous: String, failure: => A)(success: (IDENT, Loc) => A): A =
    yeetSpaces match
    case (tok @ (id: IDENT), loc) :: _ =>
      consume
      success(id, loc)
    case (tok, loc) :: _ => 
      err((msg"Expected identifiers after ${previous}; found ${tok.describe} instead" -> S(loc) :: Nil))
      failure
    case Nil => 
      err((msg"Expected identifiers after ${previous}; found end of input instead" -> lastLoc :: Nil))
      failure

  def expectKeyword[A](keyword: Keyword, previous: String, failure: => A)(success: (Stroken, Loc) => A): A =
    yeetSpaces match
    case (tok @ context.KW(keyword), loc) :: _ =>
      consume
      success(tok, loc)
    case (tok, loc) :: _ => 
      err((msg"Expected `${keyword.name}` after ${previous}; found ${tok.describe} instead" -> S(loc) :: Nil))
      failure
    case Nil => 
      err((msg"Expected `${keyword.name}` after ${previous}; found end of input instead" -> lastLoc :: Nil))
      failure
  
  def blockOf(rule: ParseRule[Tree]): Ls[Tree] = wrap(rule.name):
    cur match
    case Nil => Nil
    case (SPACE, _) :: _ => consume; blockOf(rule)
    case (tok @ context.KW(context.`keyword`), loc) :: _ =>
      lazy val rest = blockContOf(context.prefixRules)
      parseKeyword.fold(rest)(_ :: rest)
    case (tok @ context.KW(context.`define`), loc) :: _ =>
      lazy val rest = blockContOf(context.prefixRules)
      parseDefine.fold(rest)(_ :: rest)
    case (tok @ (id: IDENT), loc) :: _ => parseIdent(id, loc, rule) :: blockContOf(context.prefixRules)
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

  def parseKeyword: Option[Tree] =
    @tailrec
    def parseKeywordCont: Unit = yeetSpaces match
      case (COMMA, _) :: _ =>
        consume
        yeetSpaces match
        case (id: IDENT, _) :: _ => consume; context += id.name; parseKeywordCont
        case _ => ()
      case _ => ()
    consume
    expectIdent("`keyword`", Some(errExpr)): (id, loc) =>
      context += id.name
      parseKeywordCont
      None

  def parseDefine: Option[Tree] =
    def parseRhs: Ls[Tree] => Tree =
      val tree = expr(0)
      (args) => subst(tree, args.toArray)
    def parseLhs: Alt[Ls[Tree]] = wrap("define lhs"):
      yeetSpaces match
      case (tok @ context.KW(context.`~>`), loc) :: _ =>
        consume
        Alt.End(Nil)
      case (tok @ (id: IDENT), loc) :: _ =>
        consume
        context.getKeyword(id.name) match
        case S(kw) =>
          Alt.Kw(kw)(ParseRule(s"after ${kw.name}")(parseLhs))
        case N =>
          if id.name.isCapitalized then
            id.name match
            case "Ident" => Alt.Ident(ParseRule(s"after Ident")(parseLhs))(_ :: _)
            // case "Lit" => Alt.Const(ParseRule(s"after Ident")(parseLhs))(_ :: _)
            case "Expr" => Alt.Expr(ParseRule(s"after Expr")(parseLhs))(_ :: _)
            case _ =>
              err((msg"Expected a keyword or `~>` after `define`; found ${id.name} instead" -> S(loc) :: Nil))
              Alt.End(Nil: List[Tree])
          else
            err((msg"Expected a keyword or `~>` after `define`; found ${id.name} instead" -> S(loc) :: Nil))
            Alt.End(Nil: List[Tree])
      case (tok, loc) :: _ =>
        err((msg"Expected an identifier or `~>` after `define`; found ${tok.describe} instead" -> S(loc) :: Nil))
        consume
        Alt.End(Nil: List[Tree])
      case Nil =>
        err((msg"Expected an identifier or `~>` after `define`; found end of input instead" -> lastLoc :: Nil))
        Alt.End(Nil: List[Tree])
    consume
    expectIdent("`define`", Some(errExpr)): (id, loc) =>
      expectKeyword(context.`::=`, "the name", Some(errExpr)): (tok, loc) =>
        context += id.name -> parseLhs.map(parseRhs)
        None

  def parseIdent[A <: Tree](id: IDENT, loc: Loc, rule: ParseRule[A]): Tree = wrap("parseIdent"):
    // Checks keywords first, then identifiers, then expressions.
    context.getKeyword(id.name) match
    case S(kw) =>
      consume
      rule.kwAlts.get(kw.name) match
      case S(subRule) =>
        parseRule(CommaPrecNext, subRule).getOrElse(errExpr)
      case N =>
        // No rules begin with this keyword. Try `identAlt`.
        rule.identAlt match
        case S(identAlt) =>
          parseRule(CommaPrecNext, identAlt.rest)
            .map(res => identAlt.k(Tree.Var(id.name), res))
            .getOrElse(errExpr)
        case N =>
          // No rules begin with an identifer. Try `exprAlt`.
          // TODO dedup this common-looking logic:
          rule.exprAlt match
          case S(exprAlt) =>
            context.prefixRules.kwAlts.get(kw.name) match
            case S(subRule) =>
              val e = parseRule(CommaPrecNext, subRule).getOrElse(errExpr)
              parseRule(CommaPrecNext, exprAlt.rest).map(res => exprAlt.k(e, res)).getOrElse(errExpr)
            case N =>
              // TODO dedup?
              err((msg"Expected ${rule.whatComesAfter} after ${rule.name}; found ${id.describe} instead" -> S(loc) :: Nil))
              errExpr
          case N =>
            // No rules begin with an expression. Report an error.
            err((msg"Expected ${rule.whatComesAfter} after ${rule.name}; found ${id.describe} instead" -> S(loc) :: Nil))
            errExpr
    case N =>
      tryParseExp(CommaPrecNext, id, loc, rule).getOrElse(errExpr)
  
  private def tryParseExp[A](prec: Int, tok: Token, loc: Loc, rule: ParseRule[A]): Opt[A] =
    rule.exprAlt match
      case S(exprAlt) =>
        val e = expr(prec)
        parseRule(prec, exprAlt.rest).map(res => exprAlt.k(e, res))
      case N =>
        rule.emptyAlt match
        case S(res) => S(res)
        case N =>
          err((msg"Expected 1 ${rule.whatComesAfter} after ${rule.name}; found ${tok.describe} instead" -> S(loc) :: Nil))
          N
  
  /** A result of None means there was an error (already reported) and nothing could be parsed. */
  def parseRule[A](prec: Int, rule: ParseRule[A]): Opt[A] = wrap(prec, rule):
    def tryEmpty(tok: Token, loc: Loc) = rule.emptyAlt match
      case S(res) => S(res)
      case N =>
        consume
        err((msg"Expected ${rule.whatComesAfter} after ${rule.name}; found ${tok.describe} instead" -> S(loc) :: Nil))
        N
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
                tryEmpty(tok, loc)
            case N =>
              tryEmpty(tok, loc)
      case N =>
        // Not a keyword. Try `identAlt`.
        printDbg(s"try indent alt for ${id.name}")
        rule.identAlt match
        case S(identAlt) =>
          consume
          parseRule(prec, identAlt.rest).map(res => identAlt.k(Tree.Var(id.name), res))
        case N =>
          tryParseExp(prec, tok, loc, rule)
    case (tok @ (SEMI | COMMA), l0) :: _ =>
      // TODO(cur)
      rule.emptyAlt match
        case S(res) => S(res)
        case N =>
          err((msg"Expected ${rule.whatComesAfter} after ${rule.name}; found ${tok.describe} instead" -> lastLoc :: Nil))
          N
    case (tok, loc) :: _ =>
      tryParseExp(prec, tok, loc, rule)
      // TODO(tok)
    case Nil =>
      rule.emptyAlt match
        case S(res) =>
          S(res)
        case N =>
          err((msg"Expected ${rule.whatComesAfter} after ${rule.name}; found end of input instead" -> lastLoc :: Nil))
          N
  
  def expr(prec: Int)(using Line): Tree = wrap(prec)(exprImpl(prec))
  def exprImpl(prec: Int): Tree =
    yeetSpaces match
    case (IDENT(nme, sym), loc) :: _ =>
      printDbg(s"IDENT: $nme")
      consume
      exprCont(Tree.Var(nme).withLoc(loc), prec, allowNewlines = true)
    case (LITVAL(lit), loc) :: _ =>
      consume
      exprCont(lit.asTree, prec, allowNewlines = true)
    case (BRACKETS(Round, toks), loc) :: _ if toks.forall(_ is SPACE) =>
      consume
      val res = Tree.Const(Literal.UnitLit).withLoc(S(loc))
      exprCont(res, prec, allowNewlines = true)
    case (BRACKETS(Round, toks), loc) :: _ =>
      consume
      val res = rec(toks, S(loc), "parenthesized expression").concludeWith(_.expr(0))
      exprCont(res, prec, allowNewlines = true)
    case (tok, loc) :: _ =>
      err((msg"Expected an expression; found ${tok.describe} instead" -> lastLoc :: Nil))
      errExpr
    case Nil =>
      err((msg"Expected an expression; found end of input instead" -> lastLoc :: Nil))
      errExpr
  
  
  import Tree.*
  
  final def exprCont(acc: Tree, prec: Int, allowNewlines: Bool): Tree = wrap(prec, s"`$acc`", allowNewlines):
    cur match {
      case (COMMA, l0) :: _ if prec === 0 =>
        consume
        val rhs = expr(prec)
        Apps(Var(",").withLoc(S(l0)), acc, rhs)
        // App(Var(",").withLoc(S(l0)), PlainTup(acc, rhs))
        /* 
      case (KEYWORD(opStr @ "=>"), l0) :: (NEWLINE, l1) :: _ if opPrec(opStr)._1 > prec =>
        consume
        val rhs = Blk(typingUnit.entities)
        R(Lam(PlainTup(acc), rhs))
        */
      case (context.KW(kw @ context.`=>`), l0) :: _ if kw.leftPrecOrMin > prec =>
        consume
        val rhs = expr(kw.rightPrecOrMin)
        val res = Lam(acc, rhs)
        exprCont(res, prec, allowNewlines)
        /* 
      case (IDENT(".", _), l0) :: (br @ BRACKETS(Square, toks), l1) :: _ =>
        consume
        consume
        val idx = rec(toks, S(br.innerLoc), br.describe)
          .concludeWith(_.expr(0, allowSpace = true))
        val newAcc = Subs(acc, idx).withLoc(S(l0 ++ l1 ++ idx.toLoc))
        exprCont(newAcc, prec, allowNewlines)
        */
      case (context.OP(opStr), l0) :: _ if /* isInfix(opStr) && */ opPrec(opStr)._1 > prec =>
        consume
        val v = Var(opStr).withLoc(S(l0))
        val rhs = expr(opPrec(opStr)._2)
        exprCont(opStr match {
          case "with" =>
            rhs match {
              // TODO?
              // case rhs: Rcd =>
              //   With(acc, rhs)//.withLocOf(term)
              // case Bra(true, rhs: Rcd) =>
              //   With(acc, rhs)//.withLocOf(term)
              case _ =>
                err(msg"record literal expected here; found ${rhs.describe}" -> rhs.toLoc :: Nil)
                acc
            }
          case _ => Apps(v, acc, rhs) // App(v, PlainTup(acc, rhs))
        }, prec, allowNewlines)
        /*
      case (KEYWORD(":"), l0) :: _ if prec <= NewParser.prec(':') =>
        consume
        R(Asc(acc, typ(0)))
      case (KEYWORD("where"), l0) :: _ if prec <= 1 =>
        consume
        val tu = typingUnitMaybeIndented
        val res = Where(acc, tu.entities).withLoc(S(l0))
        exprCont(res, prec, allowNewlines = false)
        */
      case (SPACE, l0) :: _ =>
        consume
        exprCont(acc, prec, allowNewlines)
        /*
      case (SELECT(name), l0) :: _ => // TODO precedence?
        consume
        exprCont(Sel(acc, Var(name).withLoc(S(l0))), prec, allowNewlines)
      // case (br @ BRACKETS(Indent, (SELECT(name), l0) :: toks), _) :: _ =>
      case (br @ BRACKETS(Indent, (SELECT(name), l0) :: toks), _) :: _ if prec <= 1 =>
        consume
        val res = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.exprCont(Sel(acc, Var(name).withLoc(S(l0))), 0, allowNewlines = true))
        if (allowNewlines) res match {
          case L(ifb) => L(ifb) // TODO something else?
          case R(res) => exprCont(res, 0, allowNewlines)
        }
        else res
      case (br @ BRACKETS(Indent, (IDENT(opStr, true), l0) :: toks), _) :: _ =>
        consume
        rec(toks, S(br.innerLoc), br.describe).concludeWith(_.opBlock(acc, opStr, l0))
      case (KEYWORD("then"), _) :: _ if /* expectThen && */ prec === 0 =>
      // case (KEYWORD("then"), _) :: _ if /* expectThen && */ prec <= 1 =>
        consume
        L(IfThen(acc, exprOrBlockContinuation))
      case (NEWLINE, _) :: (KEYWORD("then"), _) :: _ if /* expectThen && */ prec === 0 =>
        consume
        consume
        L(IfThen(acc, exprOrBlockContinuation))
      case (NEWLINE, _) :: _ if allowNewlines =>
        consume
        exprCont(acc, 0, allowNewlines)
        
      case (br @ BRACKETS(Curly, toks), loc) :: _ if prec <= AppPrec =>
        consume
        val tu = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.typingUnitMaybeIndented).withLoc(S(loc))
        exprCont(Rft(acc, tu), prec, allowNewlines)
        
      case (COMMA | SEMI | NEWLINE | KEYWORD("then" | "else" | "in" | "=" | "do")
        | OP(_) | BRACKETS(Curly, _), _) :: _ => R(acc)
      
      case (KEYWORD("of"), _) :: _ if prec <= 1 =>
        consume
        val as = argsMaybeIndented()
        val res = App(acc, Tup(as))
        exprCont(res, prec, allowNewlines)
      case (br @ BRACKETS(Indent, (KEYWORD("of"), _) :: toks), _) :: _ if prec <= 1 =>
        consume
        // 
        // val as = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.argsMaybeIndented())
        // val res = App(acc, Tup(as))
        // exprCont(res, 0, allowNewlines = true) // ?!
        // 
        val res = rec(toks, S(br.innerLoc), br.describe).concludeWith { nested =>
          val as = nested.argsMaybeIndented()
          nested.exprCont(App(acc, Tup(as)), 0, allowNewlines = true)
        }
        // if (allowNewlines) 
        res match {
          case L(ifb) => L(ifb) // TODO something else?
          case R(res) => exprCont(res, 0, allowNewlines)
        }
        // else res
        
      case (BRACKETS(Indent, (KEYWORD("then"|"else"), _) :: toks), _) :: _ => R(acc)
      
      /* 
      case (br @ BRACKETS(Indent, toks), _) :: _ 
      if prec === 0 && !toks.dropWhile(_._1 === SPACE).headOption.map(_._1).contains(KEYWORD("else")) // FIXME
      =>
        consume
        val res = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.blockTerm)
        R(App(acc, res))
      */
      // case (br @ BRACKETS(Indent, (BRACKETS(Round | Square, toks1), _) :: toks2), _) :: _ =>
      case (br @ BRACKETS(Indent, toks @ (BRACKETS(Round | Square, _), _) :: _), _) :: _ if prec <= 1 =>
        consume
        val res = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.exprCont(acc, 0, allowNewlines = true))
        res match {
          case L(ifb) => L(ifb) // TODO something else?
          case R(res) => exprCont(res, 0, allowNewlines)
        }
        
      case (br @ BRACKETS(Angle | Square, toks), loc) :: _ =>
        consume
        val as = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.argsMaybeIndented())
        // val res = TyApp(acc, as.map(_.mapSecond.to))
        val res = TyApp(acc, as.map {
          case (N, Fld(FldFlags(false, false, _), trm)) => trm.toType match {
            case L(d) => raise(d); Top // TODO better
            case R(ty) => ty
          }
          case _ => ???
        }).withLoc(acc.toLoc.fold(some(loc))(_ ++ loc |> some))
        exprCont(res, prec, allowNewlines)
        
      /*case (br @ BRACKETS(Square, toks), loc) :: _ => // * Currently unreachable because we match Square brackets as tparams
        consume
        val idx = rec(toks, S(br.innerLoc), "subscript").concludeWith(_.expr(0))
        val res = Subs(acc, idx.withLoc(S(loc)))
        exprCont(res, prec, allowNewlines)*/
      */
      case (br @ BRACKETS(Round, toks), loc) :: _ if prec <= AppPrec =>
        consume
        // val as = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.blockMaybeIndented)
        val as = rec(toks, S(br.innerLoc), br.describe).concludeWith(_.expr(0))
        val res = Apps(acc, as)
        exprCont(res, prec, allowNewlines)
      // case (KEYWORD(Keyword.`of`), _) :: _ =>
      //   consume
      //   val as = blockMaybeIndented
      //   val res = App(acc, Tup(as))
      //   exprCont(res, prec, allowNewlines)
      /*
      case c @ (h :: _) if (h._1 match {
        case KEYWORD(":" | "of" | "where" | "extends") | SEMI | BRACKETS(Round | Square, _)
          | BRACKETS(Indent, (
              KEYWORD("of") | SEMI
              | BRACKETS(Round | Square, _)
              | SELECT(_)
            , _) :: _)
          => false
        case _ => true
      }) =>
        val as = argsMaybeIndented()
        val res = App(acc, Tup(as))
        raise(WarningReport(msg"Paren-less applications should use the 'of' keyword"
          -> res.toLoc :: Nil, newDefs = true))
        exprCont(res, prec, allowNewlines)
        */
        
      // No need for now because infix operators are handled by `opPrec` for now.
      // case (KEYWORD(kw), l0) :: _ if kw.leftPrecOrMin > prec =>
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
    }

  /** A brute-force substitution disregarding any existing abstractions */
  def subst(t: Tree, args: Array[Tree]): Tree = t match
    case t @ Tree.Var(nme) if nme.startsWith("$") =>
      nme.tail.toIntOption match
      case Some(n) if 1 <= n && n <= args.length => args(n - 1)
      case _ => warn(msg"Invalid meta-variable reference ${nme}" -> t.toLoc :: Nil); t
    case Tree.App(f, a) => Tree.App(subst(f, args), subst(a, args))
    case Tree.Lam(lhs, rhs) => Tree.Lam(subst(lhs, args), subst(rhs, args))
    case _ => t

end Parser
