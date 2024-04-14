package hkmc2
package syntax

import sourcecode.{Name, Line}
import mlscript.utils.*, shorthands.*
import hkmc2.Message._
import BracketKind._


enum Alt[+A]:
  case Kw[Rest](kw: Keyword)(val rest: ParseRule[Rest]) extends Alt[Rest]
  case Ident[Rest, +Res](val rest: ParseRule[Rest])(val k: (Tree.Var, Rest) => Res) extends Alt[Res]
  // Const is less useful than Ident.
  // case Const[Rest, +Res](val rest: ParseRule[Rest])(val k: (Tree.Const, Rest) => Res) extends Alt[Res]
  case Expr[Rest, +Res](rest: ParseRule[Rest])(val k: (Tree, Rest) => Res) extends Alt[Res]
  case Blk[Rest, +Res](rest: ParseRule[Rest])(val k: (Tree, Rest) => Res) extends Alt[Res]
  case End(a: A)

  def restOption: Option[ParseRule[?]] = this match
    case k: Kw[?] => Some(k.rest)
    case Ident(rest) => Some(rest)
    // case Const(rest) => Some(rest)
    case Expr(rest) => Some(rest)
    case Blk(rest) => Some(rest)
    case End(_) => None
  
  def map[B](f: A => B): Alt[B] = 
    this match
    case k: Kw[?] => Kw(k.kw)(k.rest.map(f))
    case i: Ident[?, ?] => Ident(i.rest)((str, rest) => f(i.k(str, rest)))
    // case c: Const[?, ?] => Const(c.rest)((lit, rest) => f(c.k(lit, rest)))
    case e: Expr[rest, A] => Expr(e.rest)((tree, rest) => f(e.k(tree, rest)))
    case End(a) => End(f(a))
    case b: Blk[rest, A] => Blk(b.rest)((tree, rest) => f(b.k(tree, rest)))

  override def toString(): String =
    val head = this match 
      case alt @ Kw(kw) => s"`${kw.name}`"
      case alt @ Ident(_) => "Ident"
      // case alt @ Const(_) => "Const"
      case alt @ Expr(_) => "Expr"
      case alt @ Blk(_) => "Block"
      case End(_) => "End"
    head + " " + restOption.fold("")(_.altsToString)
    

class ParseRule[+A](val name: Str)(alts: Alt[A]*):
  def map[B](f: A => B): ParseRule[B] =
    ParseRule(name)(alts.map(_.map(f))*)

  def |[B >: A](that: Alt[B]): ParseRule[B] =
    ParseRule(name)(this.alts :+ that: _*)

  def altsToString: String = alts.mkString(" | ")
  
  override def toString: Str = s"$name ::= " + altsToString
  
  lazy val emptyAlt = alts.collectFirst { case Alt.End(a) => a }
  lazy val kwAlts = alts.collect { case k @ Alt.Kw(kw) => kw.name -> k.rest }.toMap
  lazy val identAlt = alts.collectFirst { case alt: Alt.Ident[rst, A] => alt }
  lazy val exprAlt = alts.collectFirst { case alt: Alt.Expr[rst, A] => alt }
  lazy val blkAlt = alts.collectFirst { case alt: Alt.Blk[rst, A] => alt }
  
  def whatComesAfter: Str =
    alts.map:
      case Alt.Kw(kw) => s"'${kw.name}' keyword"
      case Alt.Ident(rest) => "identifier"
      // case Alt.Const(rest) => "constant"
      case Alt.Expr(rest) => "expression"
      case Alt.Blk(rest) => "indented block"
      case Alt.End(_) => "end of input"
    .toList
    match
      case Nil => "nothing at all"
      case str :: Nil => str
      case str1 :: str2 :: Nil => s"$str1 or $str2"
      case strs => strs.init.mkString(", ") + ", or " + strs.last
