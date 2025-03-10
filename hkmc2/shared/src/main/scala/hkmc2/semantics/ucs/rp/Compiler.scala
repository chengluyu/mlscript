package hkmc2
package semantics
package ucs.rp
import mlscript.utils.*, shorthands.*
import collection.mutable.Buffer

enum MatchFunction:
  case Forwarded(impl: Implemented)
  case Implemented(inputs: Ls[Pattern])
  
  def matches(patterns: Ls[Pattern]): Opt[Map[Int, Int]] = ???

class Compiler(elaborator: Elaborator):
  import elaborator.tl.*
  
  // We don't use `Map` because the cache is based on subsequence.
  private val cache = Buffer.empty[MatchFunction]
  
  def buildMatchFunction(patterns: Ls[Pattern]): Unit = ???
