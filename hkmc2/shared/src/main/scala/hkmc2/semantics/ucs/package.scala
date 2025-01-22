package hkmc2
package semantics

import collection.mutable.Buffer
import syntax.Tree, Tree.StrLit
import mlscript.utils._, shorthands._
import Message.MessageContext

package object ucs:
  def error(msgs: (Message, Option[Loc])*)(using Raise): Unit =
    raise(ErrorReport(msgs.toList))
  
  def warn(msgs: (Message, Option[Loc])*)(using Raise): Unit =
    raise(WarningReport(msgs.toList))
    
  def incompatibleRangeType(lower: Tree, upper: Tree)(using Raise): Unit =
    val loc = lower.toLoc.fold(upper.toLoc)(_ ++ upper.toLoc |> some)
    error(msg"Range bounds must be of the same type." -> loc)
    
  def isValidCharacterRange(lo: StrLit, hi: StrLit)(using Raise): Bool =
    val ds = Buffer.empty[(Message, Option[Loc])]
    if lo.value.length != 1 then
      ds += msg"The lower bound of the character range must be a single character." -> lo.toLoc
    if hi.value.length != 1 then
      ds += msg"The upper bound of the character range must be a single character." -> hi.toLoc
    if ds.nonEmpty then error(ds.toSeq*)
    ds.isEmpty
end ucs
