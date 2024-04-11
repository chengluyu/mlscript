package hkmc2.syntax

import collection.mutable.Map as MutMap
import collection.mutable.Set as MutSet
import scala.collection.mutable.ListBuffer

class Context:
  private val keywords: MutMap[String, Keyword] = MutMap.empty
  private val rules: MutMap[String, ParseRule[Tree]] = MutMap.empty
  private val topLevelRules: ListBuffer[Alt[Tree]] = ListBuffer.empty
  private val atomicSymbols: MutSet[String] = MutSet.empty

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

  atomicSymbols += "Ident"
  atomicSymbols += "Expr"

  def +=(kw: Keyword): Unit =
    keywords += kw.name -> kw

  def +=(name: String): Unit =
    val kw = Keyword(name, None, None)
    keywords += name -> kw

  def +=(alt: Alt[Tree]): Unit =
    topLevelRules += alt

  def +=(alt: ParseRule[Tree]): Unit =
    rules += alt.name -> alt

  // Getters

  @inline def getKeyword(name: String): Option[Keyword] = keywords.get(name)
  @inline def hasKeyword(name: String): Boolean = keywords.contains(name)
  def listKeywords: Iterator[String] = keywords.keysIterator
  def listRules: Iterator[ParseRule[Tree]] = rules.valuesIterator

  def prefixRules: ParseRule[Tree] = ParseRule("start of statement")(topLevelRules.toSeq: _*)

  object KW:
    def unapply(t: Token): Option[Keyword] = t match
      case IDENT(name, _) => keywords.get(name)
      case _ => None
  
  object OP:
    def unapply(t: Token): Option[String] = t match
      case IDENT(nme, true) if !keywords.contains(nme) => Some(nme)
      case _ => None

end Context
