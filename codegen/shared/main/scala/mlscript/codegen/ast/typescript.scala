package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class Noop()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Miscellaneous

case class TSParameterProperty(val parameter: Identifier | AssignmentPattern)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with LVal:
  var accessibility: Option[AccessModifier] = None
  var decorators: Option[List[Decorator]] = None
  var `override`: Option[Boolean] = None
  var readonly: Option[Boolean] = None

case class TSDeclareFunction(
  val id: Option[Identifier] = None,
  val typeParameters: Option[TSTypeParameterDeclaration | Noop] = None,
  val params: List[Identifier | Node with Pattern | RestElement],
  val returnType: Option[TSTypeAnnotation | Noop] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement with Declaration:
  var async: Option[Boolean] = None
  var declare: Option[Boolean] = None
  var generator: Option[Boolean] = None

enum TSDeclareMethodKind:
  case Getter
  case Setter
  case Method
  case Constructor

case class TSDeclareMethod(
  val decorators: Option[List[Decorator]] = None,
  val key: Identifier | StringLiteral | NumericLiteral | BigIntLiteral | Node with Expression,
  val typeParameters: Option[TSTypeParameterDeclaration | Noop] = None,
  val params: List[Identifier | Node with Pattern | RestElement | TSParameterProperty],
  val returnType: Option[TSTypeAnnotation | Noop] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript:
  var `abstract`: Option[Boolean] = None
  var access: Option[AccessModifier] = None
  var accessibility: Option[AccessModifier] = None
  var async: Option[Boolean] = None
  var computed: Option[Boolean] = None
  var generator: Option[Boolean] = None
  var kind: Option[TSDeclareMethodKind] = None
  var optional: Option[Boolean] = None
  var `override`: Option[Boolean] = None
  var static: Option[Boolean] = None

case class TSQualifiedName(val left: Node with TSEntityName, val right: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSEntityName

case class TSCallSignatureDeclaration(
  val typeParameters: Option[TSTypeParameterDeclaration] = None,
  val parameters: List[Identifier | RestElement],
  val typeAnnotation: Option[TSTypeAnnotation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement

case class TSConstructSignatureDeclaration(
  val typeParameters: Option[TSTypeParameterDeclaration] = None,
  val parameters: List[Identifier | RestElement],
  val typeAnnotation: Option[TSTypeAnnotation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement

enum TSPropertySignatureKind:
  case Getter
  case Setter

case class TSPropertySignature(
  val key: Node with Expression,
  val typeAnnotation: Option[TSTypeAnnotation] = None,
  val initializer: Option[Node with Expression] = None,
  val kind: TSPropertySignatureKind
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement:
  var computed: Option[Boolean] = None
  var optional: Option[Boolean] = None
  var readonly: Option[Boolean] = None

enum TSMethodSignatureKind:
  case Method
  case Getter
  case Setter

case class TSMethodSignature(
  val key: Node with Expression,
  val typeParameters: Option[TSTypeParameterDeclaration] = None,
  val parameters: List[Identifier | RestElement],
  val typeAnnotation: Option[TSTypeAnnotation] = None,
  val kind: TSMethodSignatureKind
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement:
  var computed: Option[Boolean] = None
  var optional: Option[Boolean] = None

case class TSIndexSignature(
  val parameters: List[Identifier],
  val typeAnnotation: Option[TSTypeAnnotation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement:
  var readonly: Option[Boolean] = None
  var static: Option[Boolean] = None

case class TSAnyKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSBooleanKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSBigIntKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSIntrinsicKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSNeverKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSNullKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSNumberKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSObjectKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSStringKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSSymbolKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSUndefinedKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSUnknownKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSVoidKeyword()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSThisType()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType with TSBaseType

case class TSFunctionType(
  val typeParameters: Option[TSTypeParameterDeclaration] = None,
  val parameters: List[Identifier | RestElement],
  val typeAnnotation: Option[TSTypeAnnotation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSConstructorType(
  val typeParameters: Option[TSTypeParameterDeclaration] = None,
  val parameters: List[Identifier | RestElement],
  val typeAnnotation: Option[TSTypeAnnotation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType:
  var `abstract`: Option[Boolean] = None

case class TSTypeReference(
  val typeName: Node with TSEntityName,
  val typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSTypePredicate(
  val parameterName: Identifier | TSThisType,
  val typeAnnotation: Option[TSTypeAnnotation] = None,
  val asserts: Option[Boolean] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSTypeQuery(
  val exprName: Node with TSEntityName | TSImportType,
  val typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSTypeLiteral(val members: List[Node with TSTypeElement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSArrayType(val elementType: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSTupleType(val elementTypes: List[Node with TSType | TSNamedTupleMember])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSOptionalType(val typeAnnotation: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSRestType(val typeAnnotation: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSNamedTupleMember(
  val label: Identifier,
  val elementType: Node with TSType,
  val optional: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript

case class TSUnionType(val types: List[Node with TSType])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSIntersectionType(val types: List[Node with TSType])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSConditionalType(
  val checkType: Node with TSType,
  val extendsType: Node with TSType,
  val trueType: Node with TSType,
  val falseType: Node with TSType
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSInferType(val typeParameter: TSTypeParameter)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSParenthesizedType(val typeAnnotation: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSTypeOperator(val typeAnnotation: Node with TSType, val operator: String)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSIndexedAccessType(val objectType: Node with TSType, val indexType: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSMappedType(
  val typeParameter: TSTypeParameter,
  val typeAnnotation: Option[Node with TSType] = None,
  val nameType: Option[Node with TSType] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType:
  var optional: Option[true | false | "+" | "-"] = None
  var readonly: Option[true | false | "+" | "-"] = None

case class TSLiteralType(
  val literal: NumericLiteral | StringLiteral | BooleanLiteral | BigIntLiteral | TemplateLiteral | UnaryExpression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType with TSBaseType

case class TSExpressionWithTypeArguments(
  val expression: Node with TSEntityName,
  val typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSInterfaceDeclaration(
  val id: Identifier,
  val typeParameters: Option[TSTypeParameterDeclaration] = None,
  val `extends`: Option[List[TSExpressionWithTypeArguments]] = None,
  val body: TSInterfaceBody
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement with Declaration:
  var declare: Option[Boolean] = None

case class TSInterfaceBody(val body: List[Node with TSTypeElement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript

case class TSTypeAliasDeclaration(
  val id: Identifier,
  val typeParameters: Option[TSTypeParameterDeclaration] = None,
  val typeAnnotation: Node with TSType
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement with Declaration:
  var declare: Option[Boolean] = None

case class TSInstantiationExpression(
  val expression: Node with Expression,
  val typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Expression

case class TSAsExpression(
  val expression: Node with Expression,
  val typeAnnotation: Node with TSType
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Expression with LVal with PatternLike

case class TSSatisfiesExpression(
  val expression: Node with Expression,
  val typeAnnotation: Node with TSType
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Expression with LVal with PatternLike

case class TSTypeAssertion(
  val typeAnnotation: Node with TSType,
  val expression: Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Expression with LVal with PatternLike

case class TSEnumDeclaration(val id: Identifier, val members: List[TSEnumMember])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with Statement with Declaration:
  var const: Option[Boolean] = None
  var declare: Option[Boolean] = None
  var initializer: Option[Node with Expression] = None

case class TSEnumMember(
  val id: Identifier | StringLiteral,
  val initializer: Option[Node with Expression] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript

case class TSModuleDeclaration(
  val id: Identifier | StringLiteral,
  val body: TSModuleBlock | TSModuleDeclaration
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement with Declaration:
  var declare: Option[Boolean] = None
  var global: Option[Boolean] = None

case class TSModuleBlock(val body: List[Node with Statement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with Scopable with Block with BlockParent with FunctionParent

case class TSImportType(
  val argument: StringLiteral,
  val qualifier: Option[Node with TSEntityName] = None,
  val typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

enum TSImportEqualsDeclarationKind:
  case Type
  case Value

case class TSImportEqualsDeclaration(
  val id: Identifier,
  val moduleReference: Node with TSEntityName | TSExternalModuleReference,
  val isExport: Boolean
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement:
  var importKind: Option[TSImportEqualsDeclarationKind] = None

case class TSExternalModuleReference(val expression: StringLiteral)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript

case class TSNonNullExpression(val expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with Expression with LVal with PatternLike

case class TSExportAssignment(val expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with Statement

case class TSNamespaceExportDeclaration(val id: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with Statement

case class TSTypeAnnotation(val typeAnnotation: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript

case class TSTypeParameterInstantiation(val params: List[Node with TSType])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript

case class TSTypeParameterDeclaration(val params: List[TSTypeParameter])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript

case class TSTypeParameter(
  val constraint: Option[Node with TSType] = None,
  val default: Option[Node with TSType] = None,
  val name: String
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript:
  var in: Option[Boolean] = None
  var out: Option[Boolean] = None
