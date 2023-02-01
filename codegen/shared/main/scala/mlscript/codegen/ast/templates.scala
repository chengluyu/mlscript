package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class TaggedTemplateExpression(val tag: Node with Expression, val quasi: TemplateLiteral)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Expression:
  var typeParameters: Option[TSTypeParameterInstantiation] = None

case class TemplateElement(val value: Any, val tail: Boolean = false)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized

case class TemplateLiteral(
  val quasis: List[TemplateElement],
  val expressions: List[Node with Expression | Node with TSType]
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression with Literal
