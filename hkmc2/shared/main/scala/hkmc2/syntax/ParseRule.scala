package hkmc2
package syntax

import sourcecode.{Name, Line}
import mlscript.utils.*, shorthands.*
import hkmc2.Message._
import BracketKind._


enum Alt[+A]:
  case Kw[Rest](kw: Keyword)(val rest: ParseRule[Rest]) extends Alt[Rest]
  case Ident[Rest, +Res](val rest: ParseRule[Rest])(val k: (Tree.Var, Rest) => Res) extends Alt[Res]
  case Ref[First, Rest, +Res](name: String, rule: ParseRule[First])(val rest: ParseRule[Rest])(val k: (First, Rest) => Res) extends Alt[Res]
  case Expr[Rest, +Res](rest: ParseRule[Rest])(val k: (Tree, Rest) => Res) extends Alt[Res]
  case Blk[Rest, +Res](rest: ParseRule[Rest])(val k: (Tree, Rest) => Res) extends Alt[Res]
  case End(a: A)

  def restOption: Option[ParseRule[?]] = this match
    case k: Kw[?] => Some(k.rest)
    case Ident(rest) => Some(rest)
    case alt @ Ref(_, _) => Some(alt.rest)
    case Expr(rest) => Some(rest)
    case Blk(rest) => Some(rest)
    case End(_) => None
  
  def map[B](f: A => B): Alt[B] = 
    this match
    case k: Kw[?] => Kw(k.kw)(k.rest.map(f))
    case i: Ident[?, ?] => Ident(i.rest)((str, rest) => f(i.k(str, rest)))
    case r: Ref[fst, rest, A] =>
      Ref[fst, rest, B](r.name, r.rule)(r.rest)((tree, rest) => f(r.k(tree, rest)))
    case e: Expr[rest, A] => Expr(e.rest)((tree, rest) => f(e.k(tree, rest)))
    case End(a) => End(f(a))
    case b: Blk[rest, A] => Blk(b.rest)((tree, rest) => f(b.k(tree, rest)))

  override def toString(): String =
    val head = this match 
      case alt @ Kw(kw) => s"`${kw.name}`"
      case alt @ Ident(_) => "Ident"
      case alt @ Ref(name, _) => name
      case alt @ Expr(_) => "Expr"
      case alt @ Blk(_) => "Block"
      case End(_) => "End"
    head + " " + restOption.fold("")(_.altsToString)
    

class ParseRule[+A](val name: Str)(val alts: Alt[A]*):
  def map[B](f: A => B): ParseRule[B] =
    ParseRule(name)(alts.map(_.map(f))*)

  def altsToString: String = alts.mkString(" | ")
  
  override def toString: Str = s"$name ::= " + altsToString
  
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
      case Alt.Ref(name, _) => s"reference to $name"
      case Alt.Expr(rest) => "expression"
      case Alt.Blk(rest) => "indented block"
      case Alt.End(_) => "end of input"
    .toList
    match
      case Nil => "nothing at all"
      case str :: Nil => str
      case str1 :: str2 :: Nil => s"$str1 or $str2"
      case strs => strs.init.mkString(", ") + ", or " + strs.last
