package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class TSParameterProperty(
  parameter: Identifier | AssignmentPattern,
  accessibility: Option[AccessModifier] = None,
  decorators: List[Decorator] = List(),
  `override`: Boolean = false,
  readonly: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with LVal

case class TSDeclareFunction(
  id: Option[Identifier] = None,
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  params: List[Identifier | Node with Pattern | RestElement],
  returnType: Option[TSTypeAnnotation] = None,
  async: Boolean = false,
  declare: Boolean = false,
  generator: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement with Declaration

enum TSDeclareMethodKind:
  case Getter
  case Setter
  case Method
  case Constructor

case class TSDeclareMethod(
  decorators: List[Decorator] = List(),
  key: Identifier | StringLiteral | NumericLiteral | BigIntLiteral | Node with Expression,
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  params: List[Identifier | Node with Pattern | RestElement | TSParameterProperty],
  returnType: Option[TSTypeAnnotation] = None,
  `abstract`: Boolean = false,
  access: Option[AccessModifier] = None,
  async: Boolean = false,
  computed: Boolean = false,
  generator: Boolean = false,
  kind: TSDeclareMethodKind = TSDeclareMethodKind.Method,
  optional: Boolean = false,
  `override`: Boolean = false,
  static: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript

case class TSQualifiedName(left: Node with TSEntityName, right: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSEntityName

case class TSCallSignatureDeclaration(
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  parameters: List[Identifier | RestElement],
  typeAnnotation: Option[TSTypeAnnotation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement

case class TSConstructSignatureDeclaration(
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  parameters: List[Identifier | RestElement],
  typeAnnotation: Option[TSTypeAnnotation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement

enum TSPropertySignatureKind:
  case Getter
  case Setter

case class TSPropertySignature(
  key: Node with Expression,
  typeAnnotation: Option[TSTypeAnnotation] = None,
  initializer: Option[Node with Expression] = None,
  kind: TSPropertySignatureKind,
  computed: Boolean = false,
  optional: Boolean = false,
  readonly: Boolean = false,
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement

enum TSMethodSignatureKind:
  case Method
  case Getter
  case Setter

case class TSMethodSignature(
  key: Node with Expression,
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  parameters: List[Identifier | RestElement],
  typeAnnotation: Option[TSTypeAnnotation] = None,
  kind: TSMethodSignatureKind,
  computed: Boolean = false,
  optional: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement

case class TSIndexSignature(
  parameters: List[Identifier],
  typeAnnotation: Option[TSTypeAnnotation] = None,
  readonly: Boolean = false,
  static: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSTypeElement

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
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  parameters: List[Identifier | RestElement],
  typeAnnotation: Option[TSTypeAnnotation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSConstructorType(
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  parameters: List[Identifier | RestElement],
  typeAnnotation: Option[TSTypeAnnotation] = None,
  `abstract`: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSTypeReference(
  typeName: Node with TSEntityName,
  typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSTypePredicate(
  parameterName: Identifier | TSThisType,
  typeAnnotation: Option[TSTypeAnnotation] = None,
  asserts: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSTypeQuery(
  exprName: Node with TSEntityName | TSImportType,
  typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSTypeLiteral(members: List[Node with TSTypeElement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSArrayType(elementType: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSTupleType(val elementTypes: List[Node with TSType | TSNamedTupleMember])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSOptionalType(typeAnnotation: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSRestType(typeAnnotation: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSNamedTupleMember(
  label: Identifier,
  elementType: Node with TSType,
  optional: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript

case class TSUnionType(types: List[Node with TSType])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSIntersectionType(types: List[Node with TSType])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSConditionalType(
  checkType: Node with TSType,
  extendsType: Node with TSType,
  trueType: Node with TSType,
  falseType: Node with TSType
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSInferType(typeParameter: TSTypeParameter)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSParenthesizedType(typeAnnotation: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSTypeOperator(typeAnnotation: Node with TSType, operator: String)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSIndexedAccessType(objectType: Node with TSType, indexType: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with TSType

case class TSMappedType(
  typeParameter: TSTypeParameter,
  typeAnnotation: Option[Node with TSType] = None,
  nameType: Option[Node with TSType] = None,
  optional: Option[true | "+" | "-"] = None,
  readonly: Option[true | "+" | "-"] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSLiteralType(
  literal: NumericLiteral | StringLiteral | BooleanLiteral | BigIntLiteral | TemplateLiteral | UnaryExpression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType with TSBaseType

case class TSExpressionWithTypeArguments(
  expression: Node with TSEntityName,
  typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

case class TSInterfaceDeclaration(
  id: Identifier,
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  `extends`: List[TSExpressionWithTypeArguments] = Nil,
  body: TSInterfaceBody,
  declare: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement with Declaration

case class TSInterfaceBody(body: List[Node with TSTypeElement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript

case class TSTypeAliasDeclaration(
  id: Identifier,
  typeParameters: Option[TSTypeParameterDeclaration] = None,
  typeAnnotation: Node with TSType,
  declare: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement with Declaration

case class TSInstantiationExpression(
  expression: Node with Expression,
  typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Expression

case class TSAsExpression(
  expression: Node with Expression,
  typeAnnotation: Node with TSType
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Expression with LVal with PatternLike

case class TSSatisfiesExpression(
  expression: Node with Expression,
  typeAnnotation: Node with TSType
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Expression with LVal with PatternLike

case class TSTypeAssertion(
  typeAnnotation: Node with TSType,
  expression: Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Expression with LVal with PatternLike

case class TSEnumDeclaration(
  id: Identifier,
  members: List[TSEnumMember],
  const: Boolean = false,
  declare: Boolean = false,
  initializer: Option[Node with Expression] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with Statement with Declaration

case class TSEnumMember(
  id: Identifier | StringLiteral,
  initializer: Option[Node with Expression] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript

case class TSModuleDeclaration(
  id: Identifier | StringLiteral,
  body: TSModuleBlock | TSModuleDeclaration,
  declare: Boolean = false,
  global: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement with Declaration

case class TSModuleBlock(body: List[Node with Statement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with Scopable with Block with BlockParent with FunctionParent

case class TSImportType(
  argument: StringLiteral,
  qualifier: Option[Node with TSEntityName] = None,
  typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with TSType

enum TSImportEqualsDeclarationKind:
  case Type
  case Value

case class TSImportEqualsDeclaration(
  id: Identifier,
  moduleReference: Node with TSEntityName | TSExternalModuleReference,
  isExport: Boolean,
  importKind: Option[TSImportEqualsDeclarationKind] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript with Statement

case class TSExternalModuleReference(expression: StringLiteral)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript

case class TSNonNullExpression(expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with TypeScript with Expression with LVal with PatternLike

case class TSExportAssignment(expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with TypeScript with Statement

case class TSNamespaceExportDeclaration(id: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with TypeScript with Statement

case class TSTypeAnnotation(typeAnnotation: Node with TSType)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with TypeScript

case class TSTypeParameterInstantiation(params: List[Node with TSType])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with TypeScript

case class TSTypeParameterDeclaration(val params: List[TSTypeParameter])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with TypeScript

case class TSTypeParameter(
  val constraint: Option[Node with TSType] = None,
  default: Option[Node with TSType] = None,
  val name: String,
  in: Boolean = false,
  out: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with TypeScript
