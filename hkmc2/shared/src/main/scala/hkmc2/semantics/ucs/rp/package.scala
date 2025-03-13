package hkmc2
package semantics
package ucs

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

package object rp:
  type ConstructorSymbol = ClassSymbol | ModuleSymbol
  type MatchableSymbol = ClassSymbol | ModuleSymbol | PatternSymbol
  
  def subsequence[A](twigs: Vector[A], forest: Vector[A]): Option[Vector[Int]] =
    val twigPositions = VectorBuilder[Int]()
    val twigIterator = twigs.iterator
    @annotation.tailrec
    def traverseForest(forestIndex: Int): Option[Vector[Int]] =
      if twigIterator.hasNext then
        forest.indexOf(twigIterator.next, forestIndex) match
          case -1 => None
          case foundIndex =>
            twigPositions += foundIndex
            traverseForest(foundIndex + 1)
      else Some(twigPositions.result)
    traverseForest(0)
  
  extension (n: Int)
    def toSubscriptString: String =
      @tailrec def rec(acc: String, n: Int): String = n match
        case n if n < 0 => rec("\u208b", -n)
        case 0 => acc
        case _ => rec(acc + (0x2080 + n % 10).toChar, n / 10)
      if n == 0 then "\u2080" else rec("", n)
