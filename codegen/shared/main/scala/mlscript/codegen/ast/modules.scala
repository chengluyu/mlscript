package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

enum ExportKind:
  case Type
  case Value

case class ImportSpecifier(val local: Identifier, val imported: Identifier | StringLiteral)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with ModuleSpecifier:
  var importKind: Option[ImportKind] = None

case class ImportDefaultSpecifier(val local: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with ModuleSpecifier

case class ExportDefaultSpecifier(val exported: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with ModuleSpecifier

case class ExportSpecifier(val local: Identifier, val exported: Identifier | StringLiteral)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with ModuleSpecifier:
  var exportKind: Option[ExportKind] = None

case class ExportNamespaceSpecifier(val exported: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with ModuleSpecifier

case class ExportAllDeclaration(val source: StringLiteral)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Statement with Declaration with ModuleDeclaration with ExportDeclaration:
  var assertions: Option[List[ImportAttribute]] = None
  var exportKind: Option[ExportKind] = None

case class ExportNamedDeclaration(
  val declaration: Option[Node with Declaration] = None,
  val specifiers: List[ExportSpecifier | ExportDefaultSpecifier | ExportNamespaceSpecifier] = Nil,
  val source: Option[StringLiteral] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Declaration with ModuleDeclaration with ExportDeclaration:
  var assertions: Option[List[ImportAttribute]] = None
  var exportKind: Option[ExportKind] = None

case class ExportDefaultDeclaration(
  val declaration: TSDeclareFunction | FunctionDeclaration | ClassDeclaration | Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Declaration with ModuleDeclaration with ExportDeclaration:
  var exportKind: Option["value"] = None

enum ImportKind:
  case Type
  case Value
  case TypeOf

case class ImportDeclaration(
  val specifiers: List[ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier],
  val source: StringLiteral
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Declaration with ModuleDeclaration:
  var assertions: Option[List[ImportAttribute]] = None
  var importKind: Option[ImportKind] = None
  var module: Option[Boolean] = None

case class ImportAttribute(val key: Identifier | StringLiteral, val value: StringLiteral)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node

case class ImportNamespaceSpecifier(val local: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with ModuleSpecifier
