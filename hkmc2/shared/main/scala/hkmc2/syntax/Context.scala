package hkmc2.syntax

import collection.immutable.SortedMap

given ord: Ordering[Either[Int, String]] with
  def compare(x: Either[Int, String], y: Either[Int, String]): Int =
    (x, y) match
      case (Left(a), Left(b)) => a.compareTo(b)
      case (Right(a), Right(b)) => a.compareTo(b)
      case (Left(_), Right(_)) => -1
      case (Right(_), Left(_)) => 1

case class Context(
  val keywords: SortedMap[String, Keyword] = SortedMap.empty,
  val rules: SortedMap[Either[Int, String], ParseRule[Tree]] = SortedMap.empty
):
  def +(kw: Keyword): Context = copy(keywords = keywords + (kw.name -> kw))

  def +(rule: (String, Alt[Tree])): Context =
    copy(rules = rules + (Right(rule._1) -> ParseRule(rule._1)(rule._2)))
  
  def +(rule: Alt[Tree]): Context =
    copy(rules = rules + (Left(rules.size) -> ParseRule(s"unnamed#${rules.size}")(rule)))

  def ++(kws: Iterable[Keyword]): Context =
    copy(keywords = keywords ++ kws.map(kw => kw.name -> kw))

  lazy val prefixRules: ParseRule[Tree] =
    ParseRule("all prefix rules")(rules.valuesIterator.flatMap(_.alts).toSeq: _*)

  /** Filter out parse rules starting with `Expr` and `Kw` */
  lazy val infixRules: Map[Keyword, ParseRule[Tree => Tree]] =
    (rules.valuesIterator.flatMap(_.exprAlt).flatMap:
      case la => la.rest.kwAlts.iterator.map:
        case (kw, rr) => (kw, rr.map(rhs => (lhs: Tree) => la.k(lhs, rhs)))).toMap

  def prefix(kw: Keyword): Option[ParseRule[Tree]] = prefixRules.kwAlts.get(kw)

  def keyword(id: IDENT): Option[Keyword] = keywords.get(id.name)
end Context

object Context:
  /** Create a new context with default keywords and rules */
  def default: Context = Context(
    keywords = SortedMap(
      "::=" -> Parser.KW.`::=`, "~>" -> Parser.KW.`~>`,
      "keyword" -> Parser.KW.keyword,
      "define" -> Parser.KW.define,
      "syntax" -> Parser.KW.syntax,
      "=>" -> Parser.KW.`=>`),
    rules = SortedMap(
      Left(0) -> ParseRule("default")(
        Alt.Expr(ParseRule("the expression")(Alt.End(()))):
          case (tree, _) => tree
      ))
  )

case class WithContext[+A](content: A, context: Context):
  def map[B](f: A => B): WithContext[B] = copy(content = f(content))
