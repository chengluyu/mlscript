package hkmc2
package syntax

import sourcecode.{Name, Line}
import mlscript.utils.*, shorthands.*
import BracketKind._

abstract class Alt[+A]:
  import Alt.*

  def restOption: Option[ParseRule[?]] = this match
    case k: Kw[?] => Some(k.rest)
    case Ident(rest) => Some(rest)
    case Rec(key) => None
    case Expr(rest) => Some(rest)
    case Blk(rest) => Some(rest)
    case End(_) => None
  
  def map[B](f: A => B): Alt[B]

  override def toString(): String =
    val head = this match 
      case alt @ Kw(kw) => s"`${kw.name}`"
      case alt @ Ident(_) => "Ident"
      case alt @ Rec(key) => key
      case alt @ Expr(_) => "Expr"
      case alt @ Blk(_) => "Block"
      case End(_) => "End"
    head + " " + restOption.fold("")(_.altsToString)
    
object Alt:
  case class Kw[A](kw: Keyword)(val rest: ParseRule[A]) extends Alt[A]:
    override def map[B](f: A => B): Alt.Kw[B] = this match
      case k: Kw[A] => Kw(k.kw)(k.rest.map(f))

  case class Ident[Rest, +A](val rest: ParseRule[Rest])(val k: (Tree.Var, Rest) => A) extends Alt[A]:
    override def map[B](f: A => B): Ident[Rest, B] =
      Ident(rest)((str, rest) => f(k(str, rest)))

  case class Rec[+A](val key: String)(val k: Tree => A) extends Alt[A]:
    override def map[B](f: A => B): Rec[B] = Rec(key)(tree => f(k(tree)))

  case class Expr[Rest, +A](rest: ParseRule[Rest])(val k: (Tree, Rest) => A) extends Alt[A]:
    override def map[B](f: A => B): Expr[Rest, B] = Expr(rest)((tree, rest) => f(k(tree, rest)))

  case class Blk[Rest, +A](rest: ParseRule[Rest])(val k: (Tree, Rest) => A) extends Alt[A]:
    override def map[B](f: A => B): Blk[Rest, B] = Blk(rest)((tree, rest) => f(k(tree, rest)))

  case class End[A](a: A) extends Alt[A]:
    override def map[B](f: A => B): End[B] = End(f(a))
