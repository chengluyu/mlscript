package hkmc2
package syntax

import sourcecode.{Name, Line}
import mlscript.utils.*, shorthands.*
import hkmc2.Message._
import BracketKind._
import scala.annotation.tailrec


enum Alt[+A]:
  case Kw[Rest](kw: Keyword)(val rest: ParseRule[Rest]) extends Alt[Rest]
  case Ident[Rest, +Res](val rest: ParseRule[Rest])(val k: (Tree.Var, Rest) => Res) extends Alt[Res]
  case Expr[Rest, +Res](rest: ParseRule[Rest])(val k: (Tree, Rest) => Res) extends Alt[Res]
  case Blk[Rest, +Res](rest: ParseRule[Rest])(val k: (Tree, Rest) => Res) extends Alt[Res]
  case End(a: A)
  
  def map[B](f: A => B): Alt[B] = 
    this match
    case k: Kw[?] => Kw(k.kw)(k.rest.map(f))
    case i: Ident[?, ?] => Ident(i.rest)((str, rest) => f(i.k(str, rest)))
    case e: Expr[rest, A] => Expr(e.rest)((tree, rest) => f(e.k(tree, rest)))
    case End(a) => End(f(a))
    case b: Blk[rest, A] => Blk(b.rest)((tree, rest) => f(b.k(tree, rest)))

  override def toString: String =
    def rec[X](first: Bool)(a: Alt[X]): String =
      val h = if first then "" else " "
      def t[Y](a: ParseRule[Y]) = a.alts.iterator.map(rec(false)).mkString(" | ")
      a match 
        case a @ Kw(kw) => h + kw.name + t(a.rest)
        case a @ Ident(_) => h + "‹Ident›" + t(a.rest)
        case a @ Expr(_) => h + "‹Expr›" + t(a.rest)
        case a @ Blk(_) => h + "‹Block›" + t(a.rest)
        case End(_) => ""
    rec(true)(this)
    

class ParseRule[+A](val name: Str)(val alts: Alt[A]*):
  def map[B](f: A => B): ParseRule[B] =
    ParseRule(name)(alts.map(_.map(f))*)
  
  def extend[B >: A](alt: Alt[B]): ParseRule[B] =
    ParseRule(name)((alts :+ alt)*)
  
  override def toString: Str = s"$name ::= " + alts.mkString(" | ")
  
  lazy val emptyAlt: Option[A] = alts.collectFirst { case Alt.End(a) => a }
  lazy val kwAlts: Map[String, ParseRule[A]] =
    alts.collect { case k @ Alt.Kw(kw) => kw.name -> k.rest }.toMap
  lazy val identAlt: Option[Alt.Ident[?, A]] =
    alts.collectFirst { case alt: Alt.Ident[rst, A] => alt }
  lazy val exprAlt: Option[Alt.Expr[?, A]] =
    alts.collectFirst { case alt: Alt.Expr[rst, A] => alt }
  lazy val blkAlt: Option[Alt.Blk[?, A]] =
    alts.collectFirst { case alt: Alt.Blk[rst, A] => alt }
  
  def whatComesAfter: Str =
    alts.map:
      case Alt.Kw(kw) => s"'${kw.name}' keyword"
      case Alt.Ident(rest) => "identifier"
      case Alt.Expr(rest) => "expression"
      case Alt.Blk(rest) => "indented block"
      case Alt.End(_) => "end of input"
    .toList
    match
      case Nil => "nothing at all"
      case str :: Nil => str
      case str1 :: str2 :: Nil => s"$str1 or $str2"
      case strs => strs.init.mkString(", ") + ", or " + strs.last
