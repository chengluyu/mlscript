package hkmc2
package syntax

import sourcecode.{Name, Line}
import mlscript.utils.*, shorthands.*
import BracketKind._

class ParseRule[+A](val name: String)(val description: String)(val alts: Alt[A]*):
  def map[B](f: A => B): ParseRule[B] = ParseRule(name)(description)(alts.map(_.map(f)): _*)

  def andThen[B, C](rule: ParseRule[B], fn: (A, B) => C): ParseRule[C] = ???

  def altsToString: String = alts.mkString(" | ")
  
  override def toString: Str = s"$name ::= " + altsToString
  
  def emptyAlt: Option[A] = alts.collectFirst { case Alt.End(a) => a }

  def kwAlts: Map[String, ParseRule[A]] =
    alts.collect { case k @ Alt.Kw(kw) => kw.name -> k.rest }.toMap

  def kwRule(kw: Keyword)(using context: Context): Option[ParseRule[A]] =
    val x = alts.collectFirst[Option[ParseRule[A]]]:
      case k @ Alt.Kw(`kw`) => Some(k.rest)
      case alt @ Alt.Rec(key) => context.rules.get(key) match
        case None => None // Rule not found
        case Some(rule) => rule.kwRule(kw).map(r => r.map(alt.k))
    x.flatten

  def identAlt(using context: Context): Option[Alt.Ident[?, A]] =
    alts.collectFirst[Option[Alt.Ident[?, A]]]:
      case alt: Alt.Ident[rst, A] => Some(alt)
      case alt @ Alt.Rec(key) => context.rules.get(key) match
        case Some(rule) => rule.identAlt.map(r => r.map(alt.k))
        case None => None
    .flatten

  /** Get the first `Alt` begins with `Expr`. */
  def exprAlt(using context: Context): Option[Alt.Expr[?, A]] =
    alts.collectFirst[Option[Alt.Expr[?, A]]]:
      case alt: Alt.Expr[?, A] => Some(alt)
      case alt @ Alt.Rec(key) => context.rules.get(key) match
        case Some(rule) => rule.exprAlt.map(r => r.map(alt.k))
        case None => None
    .flatten

  def blkAlt(using context: Context): Option[Alt.Blk[?, A]] =
    alts.collectFirst[Option[Alt.Blk[?, A]]]:
      case alt: Alt.Blk[rst, A] => Some(alt)
      case alt @ Alt.Rec(key) => context.rules.get(key) match
        case Some(rule) => rule.blkAlt.map(r => r.map(alt.k))
        case None => None
    .flatten

  private def initialSymbols(using context: Context): List[String] =
    import Iterator.single
    alts.iterator.flatMap:
      case Alt.Kw(kw) => single(s"'${kw.name}' keyword")
      case Alt.Ident(_) => single("identifier")
      case Alt.Rec(key) => context.rules(key).initialSymbols
      case Alt.Expr(_) => single("expression")
      case Alt.Blk(_) => single("indented block")
      case Alt.End(_) => single("end of input")
    .toList
  
  def whatComesAfter(using context: Context): Str =
    initialSymbols.distinct match
      case Nil => "nothing at all"
      case str :: Nil => str
      case str1 :: str2 :: Nil => s"$str1 or $str2"
      case strs => strs.init.mkString(", ") + ", or " + strs.last
