package hkmc2
package semantics
package ucs

import scala.annotation.tailrec

package object rp:
  type ConstructorSymbol = ClassSymbol | ModuleSymbol
  type MatchableSymbol = ClassSymbol | ModuleSymbol | PatternSymbol
  
  // TODO: Implement the logic
  def subsequence[A](longer: Vector[A], shorter: Vector[A]): Option[Vector[Int]] =
    if longer == shorter then Some(Vector.empty) else None
  
  extension (n: Int)
    def toSubscriptString: String =
      @tailrec def rec(acc: String, n: Int): String =
        if n < 0 then
          rec("\u208b", -n)
        else if n == 0 then
          acc
        else
          rec(acc + (0x2080 + n % 10).toChar, n / 10)
      if n == 0 then "\u2080" else rec("", n)
