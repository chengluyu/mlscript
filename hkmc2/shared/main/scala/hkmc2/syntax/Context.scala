package hkmc2.syntax

import collection.immutable.SortedMap

case class Context(
  val keywords: SortedMap[String, Keyword] = SortedMap.empty,
  val rules: SortedMap[String, ParseRule[Tree]] = SortedMap.empty
):
  def +(kw: Keyword): Context = copy(keywords = keywords + (kw.name -> kw))

  def +(pair: (String, List[Alt[Tree]])): Context =
    copy(rules = rules.updatedWith(pair._1):
      case Some(rule) => Some(ParseRule(rule.name)(rule.description)(rule.alts ++ pair._2: _*))
      case None => Some(ParseRule(pair._1)("")(pair._2: _*)))

  def declare(key: String, description: String): Context =
    copy(rules = rules + (key -> ParseRule(key)(description)()))

  lazy val prefixRules: ParseRule[Tree] =
    ParseRule("all")("all rules")(rules.valuesIterator.flatMap(_.alts).toSeq: _*)
end Context

object Context:
  /** Create a new context with default keywords and rules */
  def default: Context = Context(
    keywords = SortedMap(
      "::=" -> Parser.KW.`::=`, "~>" -> Parser.KW.`~>`,
      "keyword" -> Parser.KW.keyword,
      "define" -> Parser.KW.define,
      "syntax" -> Parser.KW.syntax,
      "+=" -> Parser.KW.`+=`,
      "=>" -> Parser.KW.`=>`),
    rules = SortedMap(
      "default" -> ParseRule("default")("expression statement")(
        Alt.Expr(ParseRule("the expression")("after the expression")(Alt.End(()))):
          case (tree, _) => tree
      ))
  )

case class WithContext[+A](content: A, context: Context):
  def map[B](f: A => B): WithContext[B] = copy(content = f(content))

// class PredefinedParseRule[+A](val description: Str)(mkAlts: => Alt[A]*) extends ParseRule[A](description):
//   lazy val alts = mkAlts
//   def alts(using Context): Ls[Alt] = alts
// class UserDefinedParseRule(val key: Str, val description: Str)(mkAlts: => Alt[Tree]*) extends ParseRule[Tree](description):
//   def alts(using ctx: Context): Ls[Alt] = ctx.getAlts(key)
// abstract class ParseRule[+A](val description: Str)(mkAlts: => Alt[A]*):
//   def alts(using Context): Ls[Alt]