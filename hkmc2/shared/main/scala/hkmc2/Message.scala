package hkmc2

import scala.language.implicitConversions
import syntax.Tree
import mlscript.utils._, shorthands._

final case class Message(bits: Ls[Message.Bit]):
  def show: Str = showDbg
  def typeBits: Ls[Tree] = bits.collect{ case Message.Code(t) => t }
  def showDbg: Str =
    bits.iterator.map {
      case Message.Code(trm) => s"$trm"
      case Message.Text(txt) => txt
    }.mkString
  def +(that: Message): Message = Message(bits ++ that.bits)
object Message:
  def join(msgs: Seq[Message]): Message = Message(msgs.iterator.flatMap(_.bits).toList)
  
  sealed abstract class Bit
  final case class Text(str: Str) extends Bit
  final case class Code(tree: Tree) extends Bit
  implicit def fromType(tree: Tree): Message = Message(Code(tree)::Nil)
  implicit def fromStr(str: Str): Message = Message(Text(str)::Nil)
  
  implicit class MessageContext(private val ctx: StringContext):
    def msg(inserted: Message*): Message =
      assert(inserted.length === ctx.parts.length - 1)
      val parts = ctx.parts.map(str => Text(StringContext(str).s()))
      val h = parts.head
      val t = parts.tail
      Message((h +: inserted.lazyZip(t).flatMap(_.bits :+ _)).toList)
  
