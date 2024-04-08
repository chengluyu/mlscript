package hkmc2

import scala.util.boundary

import mlscript.utils._, shorthands._

import math.Ordered.orderingToOrdered


type Raise = Diagnostic => Unit

trait Located:
  def children: List[Located]
  
  private var loc: Opt[Loc] = N
  
  def withLoc(s: Int, e: Int, ori: Origin): this.type =
    withLoc(S(Loc(s, e, ori)))
  def withLoc(loco: Opt[Loc]): this.type =
    require(loc.isEmpty)
    loc = loco
    this
  def withLocOf(that: Located): this.type = withLoc(that.toLoc)
  def toLoc: Opt[Loc] = boundary:
    if loc.isEmpty then
      def subLocs = children.iterator.flatMap(_.toLoc.iterator)
      val spanStart =
        subLocs.map(_.spanStart).minOption.getOrElse(boundary.break(N))
      val spanEnd =
        subLocs.map(_.spanEnd).maxOption.getOrElse(boundary.break(N))
      val origins = subLocs.map(_.origin).toList.distinct
      assert(origins.size === 1, origins)
      val res = S(Loc(spanStart, spanEnd, origins.head))
      val _ = withLoc(res)
      res
    else loc
  private[hkmc2] def getLoc: Opt[Loc] = ???
