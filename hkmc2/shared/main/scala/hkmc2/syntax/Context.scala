package hkmc2.syntax

import collection.mutable.SortedMap as MutSortedMap
import collection.mutable.Set as MutSet
import scala.collection.mutable.ListBuffer

class Context:
  private val keywords: MutSortedMap[String, Keyword] = MutSortedMap.empty
  private val rules: MutSortedMap[String, ParseRule[Tree]] = MutSortedMap.empty

  private var _curPrec = 2
  private def curPrec: Some[Int] = Some(_curPrec)
  private def nextPrec: Some[Int] =
    val res = _curPrec
    _curPrec += 1
    Some(res)

  val eqPrec = nextPrec
  val `::=` = Keyword("::=", eqPrec, eqPrec)
  val `~>` = Keyword("~>", eqPrec, eqPrec)
  val `keyword` = Keyword("keyword", None, None)
  val `define` = Keyword("define", None, None)
  val `=>` = Keyword("=>", nextPrec, eqPrec)

  this += `keyword`
  this += `define`
  this += `::=`
  this += `~>`
  this += `=>`

  def +=(kw: Keyword): Unit = keywords += kw.name -> kw

  def +=(name: String): Unit = this += Keyword(name, None, None)

  def +=(rule: (String, Alt[Tree])): Unit =
    rules += rule._1 -> ParseRule(rule._1)(rule._2)

  // Getters

  @inline def getKeyword(name: String): Option[Keyword] = keywords.get(name)
  @inline def hasKeyword(name: String): Boolean = keywords.contains(name)
  def listKeywords: Iterator[String] = keywords.keysIterator
  def listRules: Iterator[ParseRule[Tree]] = rules.valuesIterator

  def prefixRules: ParseRule[Tree] =
    ParseRule("all prefix rules")(rules.valuesIterator.flatMap(_.alts).toSeq: _*)

  object KW:
    def unapply(t: Token): Option[Keyword] = t match
      case IDENT(name, _) => keywords.get(name)
      case _ => None
  
  object OP:
    def unapply(t: Token): Option[String] = t match
      case IDENT(name, true) if !keywords.contains(name) => Some(name)
      case _ => None

  object RULE:
    def unapply(t: Token): Option[ParseRule[Tree]] = t match
      case IDENT(name, false) => rules.get(name)
      case _ => None

end Context
