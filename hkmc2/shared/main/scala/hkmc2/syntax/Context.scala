package hkmc2.syntax

import collection.immutable.SortedMap

case class Context(
  val keywords: SortedMap[String, Keyword] = SortedMap.empty,
  val rules: SortedMap[String, ParseRule[Tree]] = SortedMap.empty
):
  def +(kw: Keyword): Context = copy(keywords = keywords + (kw.name -> kw))

  def +(rule: (String, Alt[Tree])): Context =
    copy(rules = rules + (rule._1 -> ParseRule(rule._1)(rule._2)))

  lazy val prefixRules: ParseRule[Tree] =
    ParseRule("all prefix rules")(rules.valuesIterator.flatMap(_.alts).toSeq: _*)
end Context

object Context:
  /** Create a new context with default keywords and rules */
  def default: Context = Context(
    keywords = SortedMap(
      "::=" -> Parser.KW.`::=`, "~>" -> Parser.KW.`~>`,
      "keyword" -> Parser.KW.keyword, "define" -> Parser.KW.define,
      "=>" -> Parser.KW.`=>`),
    rules = SortedMap(
      "default" -> ParseRule("default")(
        Alt.Expr(ParseRule("the expression")(Alt.End(()))):
          case (tree, _) => tree
      ))
  )

case class WithContext[+A](content: A, context: Context):
  def map[B](f: A => B): WithContext[B] = copy(content = f(content))
