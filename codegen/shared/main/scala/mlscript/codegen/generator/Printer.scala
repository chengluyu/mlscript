package mlscript.codegen.generator

import scala.util.matching.Regex
import scala.collection.mutable.{ArrayBuffer, HashSet}
import mlscript.codegen.{Position, Location, LocationType}
import mlscript.codegen.ast._
import mlscript.codegen.generator._

class Printer(map: SourceMapBuilder) {
  private def print(node: Node, previous: List[PrintCommand] = List())(implicit indentLevel: Int) = node match {
    // BEGIN base.scala

    // END base.scala
    // ---
    // BEGIN classes.scala

    // END classes.scala
    // ---
    // BEGIN expressions.scala
    case e @ ThisExpression() =>
      previous :+ Word("this", e.start, e.end, e.location)
    case s @ Super() =>
      previous :+ Word("super", s.start, s.end, s.location)
    case i @ Import() =>
      previous :+ Word("import", i.start, i.end, i.location)
    case es @ EmptyStatement() =>
      previous :+ Semicolon(true, es.start, es.end, es.location)
    // END expressions.scala
    // ---
    // BEGIN methods.scala

    // END methods.scala
    // ---
    // BEGIN modules.scala

    // END modules.scala
    // ---
    // BEGIN statements.scala

    // END statements.scala
    // ---
    // BEGIN templates.scala

    // END templates.scala
    // ---
    // BEGIN types.scala

    // END types.scala
    // ---
    // BEGIN typescript.scala

    // END typescript.scala
  }
}

object Printer {
  def apply(map: SourceMapBuilder) = new Printer(map)
}
