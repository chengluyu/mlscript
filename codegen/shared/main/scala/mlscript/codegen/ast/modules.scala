package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

enum ImportKind:
  case Type
  case Value
  case TypeOf

enum ExportKind:
  case Type
  case Value

case class ImportSpecifier(local: Option[Identifier], imported: Identifier | StringLiteral, importKind: ImportKind)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with ModuleSpecifier

case class ImportDefaultSpecifier(local: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with ModuleSpecifier

case class ExportDefaultSpecifier(exported: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with ModuleSpecifier

case class ExportSpecifier(local: Option[Identifier], exported: Identifier | StringLiteral, exportKind: ExportKind)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with ModuleSpecifier

case class ExportNamespaceSpecifier(exported: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with ModuleSpecifier

case class ExportAllDeclaration(
  source: StringLiteral,
  assertions: Option[List[ImportAttribute]] = None,
  exportKind: ExportKind
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Statement with Declaration with ModuleDeclaration with ExportDeclaration

case class ExportNamedDeclaration(
  declaration: Option[Node with Declaration] = None,
  specifiers: List[ExportSpecifier | ExportDefaultSpecifier | ExportNamespaceSpecifier] = Nil,
  source: Option[StringLiteral] = None,
  assertions: Option[List[ImportAttribute]] = None,
  exportKind: ExportKind
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Declaration with ModuleDeclaration with ExportDeclaration

case class ExportDefaultDeclaration(
  declaration: TSDeclareFunction | FunctionDeclaration | ClassDeclaration | Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Declaration with ModuleDeclaration with ExportDeclaration

case class ImportDeclaration(
  specifiers: List[ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier],
  source: StringLiteral,
  assertions: Option[List[ImportAttribute]] = None,
  importKind: ImportKind,
  module: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Declaration with ModuleDeclaration

case class ImportAttribute(key: Identifier | StringLiteral, value: StringLiteral)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node

case class ImportNamespaceSpecifier(local: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with ModuleSpecifier
