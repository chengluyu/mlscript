package hkmc2.utils.path

import mlscript.utils.*, shorthands.*

import AbsolutePath.*
import scala.annotation.tailrec

/** A platform-agnostic absolute path. */
final case class AbsolutePath(root: AbsolutePath.Root, segments: Vector[String]):
  def last: String = segments.last
  
  def up: AbsolutePath =
    if segments.isEmpty then this
    else AbsolutePath(root, segments.dropRight(1))
  
  def relativeTo(other: AbsolutePath): String =
    require(root == other.root, "Cannot compute relative path between different roots")
    val xs = segments.iterator
    val ys = other.segments.iterator
    def go: String = (xs.nextOption, ys.nextOption) match
      case (Some(x), Some(y)) if x == y => go
      case (Some(x), Some(y)) =>
        xs.mkString((".." + root.separator).repeat(ys.size + 1), root.separator, "")
      case (Some(x), None) =>
        xs.mkString("." + root.separator + x, root.separator, "")
      case (None, Some(y)) =>
        (".." + root.separator).repeat(ys.size + 1)
      case (None, None) => "."
    go
  
  override def toString(): String =
    segments.mkString(root.toString(), root.separator, "")

object AbsolutePath:
  enum Root:
    case Unix
    case Windows(drive: String)
    
    def separator: String = this match
      case Unix => "/"
      case Windows(_) => "\\"
    
    override def toString(): String = this match
      case Unix => "/"
      case Windows(drive) => s"$drive:\\"
  
  def apply(segments: String*): AbsolutePath =
    // TODO: Handle Windows segments. What do they look like?
    AbsolutePath(Root.Unix, segments.toVector)
