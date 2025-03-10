package hkmc2
package semantics
package ucs
package rp

import mlscript.utils.*, shorthands.*
import collection.mutable.Buffer

case class TransitionTable()

enum MatchFunction:
  
  case Forwarded(inputPatterns: Vector[Pattern], target: MatchFunction)
  case Implemented(inputPatterns: Vector[Pattern], groups: Map[ConstructorSymbol, TransitionTable])
  
  def matches(patterns: Ls[Pattern]): Opt[Map[Int, Int]] = ???
  
  def build: Term = ???
