package hkmc2
package syntax

import Lexer.escapeChar

enum Literal extends Located:
  case IntLit(value: BigInt)
  case DecLit(value: BigDecimal)
  case StrLit(value: String)
  case UnitLit

  def asTree: Tree = Tree.Const(this)

  lazy val idStr: String = this match
    case IntLit(value) => value.toString
    case DecLit(value) => value.toString
    case StrLit(value) => value.iterator.map(escapeChar).mkString("\"", "", "\"")
    case UnitLit => "unit"
  
  def children: List[Located] = Nil
end Literal

enum Tree extends Located:
  case Empty
  case Var(name: String)
  case Const(value: Literal)
  case Lam(lhs: Tree, rhs: Tree)
  case App(lhs: Tree, rhs: Tree)
  
  def children: List[Tree] = this match
    case Empty | Var(_) | Const(_) => Nil
    case Lam(lhs, rhs) => List(lhs, rhs)
    case App(lhs, rhs) => List(lhs, rhs)
  
  def describe: String = this match
    case Empty => "empty"
    case Var(name) => s"variable `$name`"
    case Const(value) => s"constant ${value.idStr}"
    case Lam(lhs, rhs) => s"lambda abstraction"
    case App(lhs, rhs) => s"application"

  def print: String = this match
    case Empty => "?"
    case Var(name) => name
    case Const(value) => value.idStr
    case Lam(Var(name), rhs) => s"$name => ${rhs.print}"
    case Lam(lhs, rhs) => s"(${lhs.print}) => ${rhs.print}"
    case App(App(Var(op), lhs), rhs) if Lexer.isOp(op) =>
      val (leftPrec, rightPrec) = Parser.opPrec(op)
      s"${lhs.bracketed(Left(leftPrec))} $op ${rhs.bracketed(Right(rightPrec))}"
    case App(lhs, rhs) =>
      val left = lhs match
        case Var(name) => name
        case _ => s"(${lhs.print})"
      left + s"(${rhs.print})"

  def bracketed(outerPrec: Either[Int, Int]): String = this match
    case App(App(Var(op), lhs), rhs) if Lexer.isOp(op) =>
      val (leftPrec, rightPrec) = Parser.opPrec(op)
      val should = outerPrec match
        case Left(prec) => prec > leftPrec
        case Right(prec) => prec > rightPrec
      if should then s"($print)" else print
    case _ => print
end Tree

object Tree:
  object Apps:
    def apply(fn: Tree, args: Tree*): Tree =
      args.foldLeft(fn)(App(_, _))
    def unapplySeq(tree: Tree): Option[List[Tree]] = tree match
      case App(fn, arg) =>
        def rec(acc: List[Tree], lhs: Tree): List[Tree] = lhs match
          case App(fn, arg) => rec(arg :: acc, fn)
          case fn => fn :: acc
        Some(rec(arg :: Nil, fn))
      case _ => None
  object Lams:
    def apply(params: List[Tree], rhs: Tree): Tree = params.foldRight(rhs)(Lam(_, _))
