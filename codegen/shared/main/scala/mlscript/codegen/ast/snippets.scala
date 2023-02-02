package mlscript.codegen.ast

import mlscript.codegen.ast.shorthands._

/**
  * A set of common code snippets for hard-code needs.
  */
object snippets:
  // (() => { throw new Error("...") })()
  def makeUnreachableCase: PartialNode[CallExpression] =
    CallExpression(
      ArrowFunctionExpression(Nil, BlockStatement(
        ThrowStatement(
          CallExpression(
            Identifier("Error").get,
            StringLiteral("non-exhaustive case expression").get :: Nil
          ).get
        ).get :: Nil
      ).get, false, false).get,
      Nil
    )

  // typeof scrutinee === "typeName"
  def makeTypeOfTest(scrutinee: Node with Expression, typeName: StringLiteral): PartialNode[BinaryExpression] =
    BinaryExpression(
      BinaryOperator.StrictEqual,
      UnaryExpression(UnaryOperator.TypeOf, scrutinee).get,
      typeName
    )

  def makeInstanceOfTest(scrutinee: Node with Expression, className: Identifier): PartialNode[BinaryExpression] =
    BinaryExpression(BinaryOperator.InstanceOf, scrutinee, className)

  def makeValueOf(value: Node with Expression): PartialNode[CallExpression] =
    CallExpression(MemberExpression(value, Identifier("valueOf").get).get, Nil)

  def makeCopyObjectProperties(source: Identifier, target: Identifier): ForInStatement =
    ForInStatement(
      VariableDeclaration(
        VariableDeclarationKind.Const,
        VariableDeclarator(Identifier("key").get, None).get :: Nil
      ).get,
      source,
      BlockStatement(
        AssignmentExpression(
          "=",
          MemberExpression(target, Identifier("key").get, true).get,
          MemberExpression(source, Identifier("key").get, true).get,
        ).get.toStatement :: Nil
      ).get
    ).get

  object mock:
    object Array:
      def isArray(argument: Node with Expression): CallExpression =
        CallExpression(
          MemberExpression(Identifier("Array").get, Identifier("isArray").get).get,
          argument :: Nil,
        ).get

    object Object:
      def assign(arguments: (Node with Expression)*): CallExpression =
        CallExpression(
          MemberExpression(Identifier("Object").get, Identifier("assign").get).get,
          arguments.toList,
        ).get

      def setPrototypeOf(argument1: Node with Expression, argument2: Node with Expression): CallExpression =
        CallExpression(
          MemberExpression(Identifier("Object").get, Identifier("setPrototypeOf").get).get,
          argument1 :: argument2 :: Nil,
        ).get

      def getPrototypeOf(argument: Node with Expression): CallExpression =
        CallExpression(
          MemberExpression(Identifier("Object").get, Identifier("getPrototypeOf").get).get,
          argument :: Nil,
        ).get
