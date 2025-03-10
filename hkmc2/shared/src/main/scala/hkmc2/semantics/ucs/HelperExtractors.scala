package hkmc2
package semantics
package ucs

import syntax.{Tree, Keyword}, Tree.*
import mlscript.utils.*, shorthands.*

/** A set of extractors to help elaborate tree. */
object HelperExtractors:
  /** A helper extractor for matching the tree of `x | y`. */
  object or:
    infix def unapply(tree: Tree): Opt[(Tree, Tree)] = tree match
      case App(Ident("|"), Tup(lhs :: rhs :: Nil)) => S(lhs, rhs)
      case _ => N
  
  /** A helper extractor for matching the tree of `x as y`. */
  object as:
    infix def unapply(tree: Tree): Opt[(Tree, Tree)] = tree match
      case InfixApp(lhs, Keyword.as, rhs) => S(lhs, rhs)
      case _ => N
  
  /** A helper extractor for matching the tree of `x => y`. */
  object `=>`:
    infix def unapply(tree: Tree): Opt[(Tree, Tree)] = tree match
      case InfixApp(lhs, Keyword.`=>`, rhs) => S(lhs, rhs)
      case _ => N
  
  /** A helper extractor for matching the tree of `x ..= y` and `x ..< y`.
   *  The Boolean value indicates whether the range is inclusive.
   */
  object to:
    infix def unapply(tree: Tree): Opt[(Tree, (Bool, Tree))] = tree match
      case App(Ident("..="), Tup(lhs :: rhs :: Nil)) => S(lhs, (true, rhs))
      case App(Ident("..<"), Tup(lhs :: rhs :: Nil)) => S(lhs, (false, rhs))
      case _ => N
      
  object `~`:
    infix def unapply(tree: Tree): Opt[(Tree, Tree)] = tree match
      case App(Ident("~"), Tup(lhs :: rhs :: Nil)) => S(lhs, rhs)
      case _ => N
