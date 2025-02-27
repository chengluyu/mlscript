import runtime from "./../Runtime.mjs";
import Predef from "./../Predef.mjs";
import Option from "./../Option.mjs";
import Stack from "./../Stack.mjs";
import BetterMap from "./../BetterMap.mjs";
import TreeTracer from "./../TreeTracer.mjs";
import Iter from "./../Iter.mjs";
import Lexer from "./Lexer.mjs";
import Token from "./parsing/Token.mjs";
import TokenHelpers from "./parsing/TokenHelpers.mjs";
import Keyword from "./parsing/Keyword.mjs";
import Precedence from "./parsing/Precedence.mjs";
import Tree from "./parsing/Tree.mjs";
import ParseRule from "./parsing/ParseRule.mjs";
let Parser1;
Parser1 = class Parser {
  static #letExpression;
  static #letDefinition;
  static #whileTerm;
  static {
    let tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26, tmp27, tmp28, tmp29, tmp30, tmp31, tmp32, tmp33, tmp34, tmp35, tmp36, tmp37, tmp38, tmp39, tmp40, tmp41, tmp42, tmp43, tmp44, tmp45, tmp46, tmp47, tmp48, tmp49, tmp50, tmp51, tmp52, tmp53, tmp54, tmp55, tmp56, tmp57, tmp58, tmp59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66, tmp67, tmp68, tmp69, tmp70, tmp71, tmp72, tmp73, tmp74, tmp75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82, tmp83, tmp84, tmp85, tmp86, tmp87, tmp88, tmp89, tmp90, tmp91, tmp92, tmp93, tmp94, tmp95, tmp96, tmp97, tmp98, tmp99, tmp100, tmp101;
    tmp = new TreeTracer.TreeTracer();
    this.tracer = tmp;
    tmp1 = new BetterMap.Map();
    this.syntaxKinds = tmp1;
    tmp2 = (lhs, rhsBindings) => {
      let first1, first0, rhs, bindings, tmp102;
      if (globalThis.Array.isArray(rhsBindings) && rhsBindings.length === 2) {
        first0 = rhsBindings[0];
        first1 = rhsBindings[1];
        rhs = first0;
        bindings = first1;
        tmp102 = Tree.Infix(Precedence.Keywords._equal, lhs, rhs);
        return Stack.Cons(tmp102, bindings)
      } else {
        throw new globalThis.Error("match error");
      }
    };
    tmp3 = ParseRule.Choice.end(Stack.Nil);
    tmp4 = ParseRule.Choice.end(runtime.Unit);
    tmp5 = ParseRule.Choice.reference("let-bindings", (tail, _) => {
      return tail
    }, "let-bindings tail", tmp4);
    tmp6 = ParseRule.Choice.keyword(Precedence.Keywords._and, "separator", tmp5);
    tmp7 = ParseRule.Choice.term((rhs, tail) => {
      return [
        rhs,
        tail
      ]
    }, "right-hand side", tmp3, tmp6);
    tmp8 = ParseRule.Choice.keyword(Precedence.Keywords._equal, "equal sign", tmp7);
    tmp9 = ParseRule.Choice.term(tmp2, "left-hand side", tmp8);
    tmp10 = Parser.defineKind("let-bindings", tmp9);
    tmp11 = Parser.letBinding(true);
    Parser.#letExpression = tmp11;
    tmp12 = Parser.letBinding(false);
    Parser.#letDefinition = tmp12;
    tmp13 = (lhs, rhsTail) => {
      let first1, first0, rhs, tail, tmp102;
      if (globalThis.Array.isArray(rhsTail) && rhsTail.length === 2) {
        first0 = rhsTail[0];
        first1 = rhsTail[1];
        rhs = first0;
        tail = first1;
        tmp102 = Tree.Infix(Precedence.Keywords._thinArrow, lhs, rhs);
        return Stack.Cons(tmp102, tail)
      } else {
        throw new globalThis.Error("match error");
      }
    };
    tmp14 = ParseRule.Choice.end(Stack.Nil);
    tmp15 = ParseRule.Choice.end(runtime.Unit);
    tmp16 = ParseRule.Choice.reference("simple-matching", (tail, _) => {
      return tail
    }, "simple-matching tail", tmp15);
    tmp17 = ParseRule.Choice.keyword(Precedence.Keywords._bar, "separator", tmp16);
    tmp18 = ParseRule.Choice.term((rhs, tail) => {
      return [
        rhs,
        tail
      ]
    }, "rhs", tmp14, tmp17);
    tmp19 = ParseRule.Choice.keyword(Precedence.Keywords._thinArrow, "arrow", tmp18);
    tmp20 = ParseRule.Choice.term(tmp13, "pattern", tmp19);
    tmp21 = Parser.defineKind("simple-matching", tmp20);
    tmp22 = ParseRule.Choice.end(runtime.Unit);
    tmp23 = ParseRule.Choice.reference("pattern-list", (tail, _) => {
      return tail
    }, "pattern list tail", tmp22);
    tmp24 = ParseRule.Choice.term((head, tail) => {
      return Stack.Cons(head, tail)
    }, "pattern", tmp23);
    tmp25 = Parser.defineKind("pattern-list", tmp24);
    tmp26 = Tree.infix(Precedence.Keywords._thinArrow);
    tmp27 = ParseRule.Choice.end(Stack.Nil);
    tmp28 = ParseRule.Choice.end(runtime.Unit);
    tmp29 = ParseRule.Choice.reference("multiple-matching", (tail, _) => {
      return tail
    }, "multiple-matching tail", tmp28);
    tmp30 = ParseRule.Choice.keyword(Precedence.Keywords._bar, "separator", tmp29);
    tmp31 = ParseRule.Choice.term((rhs, _) => {
      return rhs
    }, "the right-hand side of the arrow", tmp27, tmp30);
    tmp32 = ParseRule.Choice.keyword(Precedence.Keywords._thinArrow, "the arrow symbol", tmp31);
    tmp33 = ParseRule.Choice.reference("pattern-list", tmp26, "the list of pattern", tmp32);
    tmp34 = Parser.defineKind("multiple-matching", tmp33);
    tmp35 = ParseRule.Choice.end(runtime.Unit);
    tmp36 = ParseRule.Choice.keyword(Precedence.Keywords._done, "while end", tmp35);
    tmp37 = ParseRule.Choice.term((body, _) => {
      return body
    }, "while end", tmp36);
    tmp38 = ParseRule.Choice.keyword(Precedence.Keywords._do, "while body", tmp37);
    tmp39 = ParseRule.Choice.term(Tree.While, "while body", tmp38);
    tmp40 = ParseRule.Choice.keyword(Precedence.Keywords._while, "while condition", tmp39);
    Parser.#whileTerm = tmp40;
    tmp41 = (lhs, rhs) => {
      let param0, tail, tmp102, tmp103, tmp104;
      if (rhs instanceof Tree.Tuple.class) {
        param0 = rhs.trees;
        tail = param0;
        tmp102 = Stack.Cons(lhs, tail);
        return Tree.Tuple(tmp102)
      } else {
        tmp103 = Stack.Cons(rhs, Stack.Nil);
        tmp104 = Stack.Cons(lhs, tmp103);
        return Tree.Tuple(tmp104)
      }
    };
    tmp42 = Parser.makeInfixChoice(Precedence.Keywords._comma, "term", tmp41);
    tmp43 = (lhs, rhs) => {
      let param0, tail, tmp102, tmp103, tmp104;
      if (rhs instanceof Tree.Sequence.class) {
        param0 = rhs.trees;
        tail = param0;
        tmp102 = Stack.Cons(lhs, tail);
        return Tree.Sequence(tmp102)
      } else {
        tmp103 = Stack.Cons(rhs, Stack.Nil);
        tmp104 = Stack.Cons(lhs, tmp103);
        return Tree.Sequence(tmp104)
      }
    };
    tmp44 = Parser.makeInfixChoice(Precedence.Keywords._semicolon, "term", tmp43);
    tmp45 = ParseRule.Choice.end(runtime.Unit);
    tmp46 = ParseRule.Choice.typeExpr((rhs, _) => {
      return (lhs) => {
        return Tree.Infix(Precedence.Keywords._colon, lhs, rhs)
      }
    }, "right-hand side type", tmp45);
    tmp47 = ParseRule.Choice.keyword(Precedence.Keywords._colon, "type ascription", tmp46);
    tmp48 = Option.Some(Precedence.Keywords.appPrec);
    tmp49 = ParseRule.Choice.end(runtime.Unit);
    tmp50 = ParseRule.Choice.Ref("term", (argument, _) => {
      return (callee) => {
        return Tree.App(callee, argument)
      }
    }, tmp48, Option.None, tmp49);
    tmp51 = ParseRule.rule("infix rules for expressions", tmp42, tmp44, tmp47, tmp50);
    this.termInfixRule = tmp51;
    tmp52 = Parser.funTerm();
    tmp53 = Parser.matchTerm();
    tmp54 = Parser.functionTerm();
    tmp55 = Parser.ifThenElse();
    tmp56 = Parser.makeBracketRule(Precedence.Keywords._leftRound, Precedence.Keywords._rightRound, "term", (tree) => {
      if (tree instanceof Tree.Empty.class) {
        return Tree.Tuple(Stack.Nil)
      } else {
        return tree
      }
    });
    tmp57 = (tree) => {
      let tmp102;
      if (tree instanceof Tree.Empty.class) {
        tmp102 = Tree.Sequence(Stack.Nil);
      } else {
        tmp102 = tree;
      }
      return Tree.Bracketed(Token.Square, tmp102)
    };
    tmp58 = Parser.makeBracketRule(Precedence.Keywords._leftSquare, Precedence.Keywords._rightSquare, "term", tmp57);
    tmp59 = Parser.makeBracketRule(Precedence.Keywords._leftCurly, Precedence.Keywords._rightCurly, "term", Predef.id);
    tmp60 = Parser.makeBracketRule(Precedence.Keywords._begin, Precedence.Keywords._end, "term", (tree) => {
      if (tree instanceof Tree.Empty.class) {
        return Tree.Sequence(Stack.Nil)
      } else {
        return tree
      }
    });
    tmp61 = ParseRule.Choice.Ref("term", (lhs, compose) => {
      return runtime.safeCall(compose(lhs))
    }, Option.None, Option.None, Parser.termInfixRule);
    tmp62 = ParseRule.rule("prefix rules for expressions", Parser.#letExpression, tmp52, tmp53, tmp54, tmp55, Parser.#whileTerm, Parser.forTerm, tmp56, tmp58, tmp59, tmp60, tmp61);
    this.termRule = tmp62;
    tmp63 = (lhs, rhs) => {
      let param0, tail, tmp102, tmp103, tmp104;
      if (rhs instanceof Tree.Tuple.class) {
        param0 = rhs.trees;
        tail = param0;
        tmp102 = Stack.Cons(lhs, tail);
        return Tree.Tuple(tmp102)
      } else {
        tmp103 = Stack.Cons(rhs, Stack.Nil);
        tmp104 = Stack.Cons(lhs, tmp103);
        return Tree.Tuple(tmp104)
      }
    };
    tmp64 = Parser.makeInfixChoice(Precedence.TypeKeywords._comma, "type", tmp63);
    tmp65 = Parser.makeInfixChoice(Precedence.TypeKeywords._arrow, "type", (lhs, rhs) => {
      return Tree.Infix(Precedence.TypeKeywords._arrow, lhs, rhs)
    });
    tmp66 = Parser.makeInfixChoice(Precedence.TypeKeywords._asterisk, "type", (lhs, rhs) => {
      return Tree.Infix(Precedence.TypeKeywords._asterisk, lhs, rhs)
    });
    tmp67 = Option.Some(Precedence.TypeKeywords.appPrec);
    tmp68 = ParseRule.Choice.end(runtime.Unit);
    tmp69 = ParseRule.Choice.Ref("type", (callee, _) => {
      return (argument) => {
        return Tree.App(callee, argument)
      }
    }, tmp67, Option.None, tmp68);
    tmp70 = ParseRule.rule("infix rules for types", tmp64, tmp65, tmp66, tmp69);
    this.typeInfixRule = tmp70;
    tmp71 = Parser.makeBracketRule(Precedence.TypeKeywords._leftRound, Precedence.TypeKeywords._rightRound, "type", Predef.id);
    tmp72 = ParseRule.Choice.Ref("type", (lhs, compose) => {
      return runtime.safeCall(compose(lhs))
    }, Option.None, Option.None, Parser.typeInfixRule);
    tmp73 = ParseRule.rule("rules for types", tmp71, tmp72);
    this.typeRule = tmp73;
    tmp74 = (ctor, argOpt) => {
      let param0, arg;
      if (argOpt instanceof Option.Some.class) {
        param0 = argOpt.value;
        arg = param0;
        return Tree.Infix(Precedence.TypeKeywords._of, ctor, arg)
      } else if (argOpt instanceof Option.None.class) {
        return ctor
      } else {
        throw new globalThis.Error("match error");
      }
    };
    tmp75 = ParseRule.Choice.end(Option.None);
    tmp76 = ParseRule.Choice.end(runtime.Unit);
    tmp77 = ParseRule.Choice.reference("type", (argument, _) => {
      return Option.Some(argument)
    }, "the variant constructor's argument", tmp76);
    tmp78 = ParseRule.Choice.keyword(Precedence.Keywords._of, "the `of` operator", tmp77);
    tmp79 = ParseRule.Choice.typeExpr(tmp74, "the variant constructor's name", tmp75, tmp78);
    tmp80 = Parser.defineKind("constr-decl", tmp79);
    tmp81 = Parser.defineKind("variants", Parser.variantsRule);
    tmp82 = Parser.defineKind("typedefs", Parser.typedefRule);
    tmp83 = ParseRule.Choice.end(runtime.Unit);
    tmp84 = ParseRule.Choice.reference("variants", (rhs, _) => {
      return (lhs) => {
        return Tree.Infix(Precedence.Keywords._equal, lhs, rhs)
      }
    }, "typedef-rhs: variants", tmp83);
    tmp85 = ParseRule.Choice.map(Parser.recordTypeChoice, (rhs) => {
      return (lhs) => {
        return Tree.Infix(Precedence.Keywords._equal, lhs, rhs)
      }
    });
    tmp86 = ParseRule.Choice.keyword(Precedence.Keywords._equal, "`=` operator", tmp84, tmp85);
    tmp87 = ParseRule.Choice.end(runtime.Unit);
    tmp88 = ParseRule.Choice.reference("type", (rhs, _) => {
      return (lhs) => {
        return Tree.Infix(Precedence.Keywords._equalequal, lhs, rhs)
      }
    }, "type alias body", tmp87);
    tmp89 = ParseRule.Choice.keyword(Precedence.Keywords._equalequal, "`==` operator", tmp88);
    tmp90 = Parser.defineKind("typedef-rhs", tmp86, tmp89);
    tmp91 = Parser.defineKind("label-decl", Parser.labelDecl);
    tmp92 = Parser.defineKind("label-decls", Parser.labelDecls);
    tmp93 = ParseRule.Choice.end(Stack.Nil);
    tmp94 = ParseRule.Choice.end(runtime.Unit);
    tmp95 = ParseRule.Choice.reference("constr-decls", (tail, _) => {
      return tail
    }, "more constructor declarations", tmp94);
    tmp96 = ParseRule.Choice.keyword(Precedence.Keywords._bar, "separator `and`", tmp95);
    tmp97 = ParseRule.Choice.reference("constr-decl", Stack.Cons, "the first constructor declaration", tmp93, tmp96);
    tmp98 = Parser.defineKind("constr-decls", tmp97);
    tmp99 = Parser.typeDefinition();
    tmp100 = Parser.exceptionDefinition();
    tmp101 = ParseRule.rule("prefix rules for module items", Parser.#letDefinition, tmp99, tmp100);
    this.declRule = tmp101;
  }
  static getRuleByKind(kind) {
    let tmp;
    tmp = runtime.safeCall(Parser.syntaxKinds.get(kind));
    return Option.unsafe.get(tmp)
  } 
  static defineKind(name, ...choices) {
    let tmp;
    tmp = ParseRule.rule(name, ...choices);
    return Parser.syntaxKinds.insert(name, tmp)
  } 
  static letBinding(hasInClause) {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17;
    intro = "let binding: ";
    tmp = intro + "keyword";
    tmp1 = intro + "keyword";
    tmp2 = intro + "`rec` keyword";
    tmp3 = ParseRule.Choice.end(runtime.Unit);
    tmp4 = ParseRule.Choice.keyword(Precedence.Keywords._rec, tmp2, tmp3);
    tmp5 = ParseRule.rule(tmp1, tmp4);
    tmp6 = intro + "body";
    if (hasInClause === true) {
      tmp7 = intro + "`in` keyword";
      tmp8 = intro + "body";
      tmp9 = ParseRule.Choice.end(runtime.Unit);
      tmp10 = ParseRule.Choice.term((body, _) => {
        return Option.Some(body)
      }, tmp8, tmp9);
      tmp11 = ParseRule.Choice.keyword(Precedence.Keywords._in, tmp7, tmp10);
      tmp12 = ParseRule.Choice.end(Option.None);
      tmp13 = Predef.tuple(tmp11, tmp12);
    } else {
      tmp14 = ParseRule.Choice.end(Option.None);
      tmp13 = Predef.tuple(tmp14);
    }
    tmp15 = ParseRule.Choice.reference("let-bindings", (bindings, body) => {
      return Tree.LetIn(bindings, body)
    }, "let-bindings", ...tmp13);
    tmp16 = ParseRule.rule(tmp6, tmp15);
    tmp17 = ParseRule.Choice.Optional(tmp5, tmp16);
    return ParseRule.Choice.keyword(Precedence.Keywords._let, tmp, tmp17)
  } 
  static ifThenElse() {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13;
    intro = "if-then-else: ";
    tmp = intro + "`if` keyword";
    tmp1 = (tst, conAlt) => {
      let first1, first0, con, alt;
      if (globalThis.Array.isArray(conAlt) && conAlt.length === 2) {
        first0 = conAlt[0];
        first1 = conAlt[1];
        con = first0;
        alt = first1;
        return Tree.Ternary(Precedence.Keywords._if, tst, con, alt)
      } else {
        throw new globalThis.Error("match error");
      }
    };
    tmp2 = intro + "condition";
    tmp3 = intro + "`then` keyword";
    tmp4 = intro + "consequent";
    tmp5 = ParseRule.Choice.end(Option.None);
    tmp6 = intro + "`else` keyword";
    tmp7 = intro + "alternative";
    tmp8 = ParseRule.Choice.end(Option.None);
    tmp9 = ParseRule.Choice.term((alt, _) => {
      return Option.Some(alt)
    }, tmp7, tmp8);
    tmp10 = ParseRule.Choice.keyword(Precedence.Keywords._else, tmp6, tmp9);
    tmp11 = ParseRule.Choice.term((con, optAlt) => {
      return [
        con,
        optAlt
      ]
    }, tmp4, tmp5, tmp10);
    tmp12 = ParseRule.Choice.keyword(Precedence.Keywords._then, tmp3, tmp11);
    tmp13 = ParseRule.Choice.term(tmp1, tmp2, tmp12);
    return ParseRule.Choice.keyword(Precedence.Keywords._if, tmp, tmp13)
  } 
  static funTerm() {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7;
    intro = "function expression: ";
    tmp = intro + "keyword";
    tmp1 = intro + "parameters";
    tmp2 = intro + "arrow";
    tmp3 = intro + "body";
    tmp4 = ParseRule.Choice.end(Option.None);
    tmp5 = ParseRule.Choice.term((body, _) => {
      return body
    }, tmp3, tmp4);
    tmp6 = ParseRule.Choice.keyword(Precedence.Keywords._thinArrow, tmp2, tmp5);
    tmp7 = ParseRule.Choice.term((params, body) => {
      let tmp8;
      tmp8 = Stack.Cons(params, Stack.Nil);
      return Tree.Lambda(tmp8, body)
    }, tmp1, tmp6);
    return ParseRule.Choice.keyword(Precedence.Keywords._fun, tmp, tmp7)
  } 
  static matchTerm() {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11;
    intro = "`match` expression: ";
    tmp = intro + "keyword";
    tmp1 = intro + "scrutinee";
    tmp2 = intro + "with";
    tmp3 = intro + "body";
    tmp4 = intro + "leading bar";
    tmp5 = ParseRule.Choice.end(runtime.Unit);
    tmp6 = ParseRule.Choice.keyword(Precedence.Keywords._bar, tmp4, tmp5);
    tmp7 = ParseRule.rule(tmp3, tmp6);
    tmp8 = Parser.getRuleByKind("simple-matching");
    tmp9 = ParseRule.Choice.Optional(tmp7, tmp8);
    tmp10 = ParseRule.Choice.keyword(Precedence.Keywords._with, tmp2, tmp9);
    tmp11 = ParseRule.Choice.term((scrutinee, branches) => {
      return Tree.Match(scrutinee, branches)
    }, tmp1, tmp10);
    return ParseRule.Choice.keyword(Precedence.Keywords._match, tmp, tmp11)
  } 
  static functionTerm() {
    let intro, body, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8;
    intro = "`function` definition: ";
    tmp = intro + "body";
    tmp1 = intro + "leading bar";
    tmp2 = ParseRule.Choice.end(runtime.Unit);
    tmp3 = ParseRule.Choice.keyword(Precedence.Keywords._bar, tmp1, tmp2);
    tmp4 = ParseRule.rule(tmp, tmp3);
    tmp5 = Parser.getRuleByKind("simple-matching");
    tmp6 = ParseRule.Choice.Optional(tmp4, tmp5);
    body = tmp6;
    tmp7 = intro + "`function` keyword";
    tmp8 = ParseRule.Choice.map(body, (branches) => {
      return Tree.Match(Tree.empty, branches)
    });
    return ParseRule.Choice.keyword(Precedence.Keywords._function, tmp7, tmp8)
  } 
  static get forTerm() {
    let intro, innerPart, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18;
    intro = "for: ";
    tmp = intro + "`do` keyword";
    tmp1 = intro + "body expression";
    tmp2 = intro + "`done` keyword";
    tmp3 = ParseRule.Choice.end(runtime.Unit);
    tmp4 = ParseRule.Choice.keyword(Precedence.Keywords._done, "the end", tmp3);
    tmp5 = ParseRule.Choice.term((body, _) => {
      return body
    }, tmp2, tmp4);
    tmp6 = ParseRule.Choice.keyword(Precedence.Keywords._do, tmp1, tmp5);
    tmp7 = ParseRule.Choice.term((end, body) => {
      return [
        end,
        body
      ]
    }, tmp, tmp6);
    innerPart = tmp7;
    tmp8 = intro + "left-hand side";
    tmp9 = (head, startEndBody) => {
      let first2, first1, first0, start, end, body;
      if (globalThis.Array.isArray(startEndBody) && startEndBody.length === 3) {
        first0 = startEndBody[0];
        first1 = startEndBody[1];
        first2 = startEndBody[2];
        start = first0;
        end = first1;
        body = first2;
        return Tree.For(head, start, end, body)
      } else {
        throw new globalThis.Error("match error");
      }
    };
    tmp10 = intro + "head";
    tmp11 = intro + "start expression";
    tmp12 = (start, endBody) => {
      let first1, first0, end, body;
      if (globalThis.Array.isArray(endBody) && endBody.length === 2) {
        first0 = endBody[0];
        first1 = endBody[1];
        end = first0;
        body = first1;
        return [
          start,
          end,
          body
        ]
      } else {
        throw new globalThis.Error("match error");
      }
    };
    tmp13 = intro + "`to` or `downto` keyword";
    tmp14 = ParseRule.Choice.keyword(Precedence.Keywords._to, "end expression", innerPart);
    tmp15 = ParseRule.Choice.keyword(Precedence.Keywords._downto, "end expression", innerPart);
    tmp16 = ParseRule.Choice.term(tmp12, tmp13, tmp14, tmp15);
    tmp17 = ParseRule.Choice.keyword(Precedence.Keywords._equal, tmp11, tmp16);
    tmp18 = ParseRule.Choice.term(tmp9, tmp10, tmp17);
    return ParseRule.Choice.keyword(Precedence.Keywords._for, tmp8, tmp18);
  } 
  static makeInfixChoice(keyword, rhsKind, compose) {
    let tmp, tmp1, tmp2, tmp3, tmp4, tmp5;
    tmp = "operator `" + keyword.name;
    tmp1 = tmp + "`";
    tmp2 = "operator `" + keyword.name;
    tmp3 = tmp2 + "` right-hand side";
    tmp4 = ParseRule.Choice.end(runtime.Unit);
    tmp5 = ParseRule.Choice.reference(rhsKind, (rhs, _) => {
      return (lhs) => {
        return runtime.safeCall(compose(lhs, rhs))
      }
    }, tmp3, tmp4);
    return ParseRule.Choice.keyword(keyword, tmp1, tmp5)
  } 
  static makeBracketRule(opening, closing, contentKind, wrapContent) {
    let tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;
    tmp = contentKind + ": bracket content";
    tmp1 = (tree, end) => {
      let param0, param1, msg, tmp7;
      if (end instanceof Tree.Error.class) {
        param0 = end.tree;
        param1 = end.message;
        if (param0 instanceof Tree.Empty.class) {
          msg = param1;
          tmp7 = runtime.safeCall(wrapContent(tree));
          return Tree.Error(tmp7, msg)
        } else {
          throw new globalThis.Error("match error");
        }
      } else if (end instanceof Tree.Empty.class) {
        return runtime.safeCall(wrapContent(tree))
      } else {
        throw new globalThis.Error("match error");
      }
    };
    tmp2 = contentKind + ": close bracket";
    tmp3 = contentKind + ": the end of bracket";
    tmp4 = ParseRule.Choice.end(Tree.empty);
    tmp5 = ParseRule.Choice.keyword(closing, tmp3, tmp4);
    tmp6 = ParseRule.Choice.reference(contentKind, tmp1, tmp2, tmp5);
    return ParseRule.Choice.keyword(opening, tmp, tmp6)
  } 
  static get recordTypeChoice() {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;
    intro = "record type: ";
    tmp = intro + "left brace";
    tmp1 = (content, _) => {
      let tmp7, tmp8;
      if (content instanceof Stack.Nil.class) {
        tmp7 = Tree.Sequence(Stack.Nil);
        return Tree.Bracketed(Token.Curly, tmp7)
      } else {
        tmp8 = Tree.Sequence(content);
        return Tree.Bracketed(Token.Curly, tmp8)
      }
    };
    tmp2 = intro + "label-decl";
    tmp3 = intro + "right brace";
    tmp4 = ParseRule.Choice.end(Tree.empty);
    tmp5 = ParseRule.Choice.keyword(Precedence.Keywords._rightCurly, tmp3, tmp4);
    tmp6 = ParseRule.Choice.reference("label-decls", tmp1, tmp2, tmp5);
    return ParseRule.Choice.keyword(Precedence.Keywords._leftCurly, tmp, tmp6);
  } 
  static typeDefinition() {
    let intro, tmp, tmp1, tmp2, tmp3;
    intro = "type definition";
    tmp = intro + "type defs";
    tmp1 = intro + "end";
    tmp2 = ParseRule.Choice.end(runtime.Unit);
    tmp3 = ParseRule.Choice.reference("typedefs", (typedefs, _) => {
      return Tree.Define(Tree.DefineKind.Type, typedefs)
    }, tmp1, tmp2);
    return ParseRule.Choice.keyword(Precedence.Keywords._type, tmp, tmp3)
  } 
  static get variantsRule() {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7;
    intro = "variant ";
    tmp = (lhs, rhsOpt) => {
      let param0, rhs;
      if (rhsOpt instanceof Option.Some.class) {
        param0 = rhsOpt.value;
        rhs = param0;
        return Tree.Infix(Precedence.Keywords._bar, lhs, rhs)
      } else {
        return lhs
      }
    };
    tmp1 = intro + "item";
    tmp2 = ParseRule.Choice.end(Option.None);
    tmp3 = intro + "bar";
    tmp4 = intro + "end";
    tmp5 = ParseRule.Choice.end(runtime.Unit);
    tmp6 = ParseRule.Choice.reference("variants", (more, _) => {
      return Option.Some(more)
    }, tmp4, tmp5);
    tmp7 = ParseRule.Choice.keyword(Precedence.Keywords._bar, tmp3, tmp6);
    return ParseRule.Choice.reference("constr-decl", tmp, tmp1, tmp2, tmp7);
  } 
  static get typedefRule() {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9;
    intro = "typedef ";
    tmp = (lhs, rhsMore) => {
      let first1, first0, rhs, more, tmp10;
      if (globalThis.Array.isArray(rhsMore) && rhsMore.length === 2) {
        first0 = rhsMore[0];
        first1 = rhsMore[1];
        rhs = first0;
        more = first1;
        tmp10 = runtime.safeCall(rhs(lhs));
        return Stack.Cons(tmp10, more)
      } else {
        throw new globalThis.Error("match error");
      }
    };
    tmp1 = intro + "name";
    tmp2 = intro + "body";
    tmp3 = ParseRule.Choice.end(Stack.Nil);
    tmp4 = intro + "and";
    tmp5 = intro + "end";
    tmp6 = ParseRule.Choice.end(runtime.Unit);
    tmp7 = ParseRule.Choice.reference("typedefs", (more, _) => {
      return more
    }, tmp5, tmp6);
    tmp8 = ParseRule.Choice.keyword(Precedence.Keywords._and, tmp4, tmp7);
    tmp9 = ParseRule.Choice.reference("typedef-rhs", (rhs, more) => {
      return [
        rhs,
        more
      ]
    }, tmp2, tmp3, tmp8);
    return ParseRule.Choice.typeExpr(tmp, tmp1, tmp9);
  } 
  static get labelDecl() {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;
    intro = "label-decl: ";
    tmp = Tree.infix(Precedence.TypeKeywords._colon);
    tmp1 = intro + "name";
    tmp2 = intro + "colon";
    tmp3 = intro + "body";
    tmp4 = ParseRule.Choice.end(runtime.Unit);
    tmp5 = ParseRule.Choice.typeExpr((rhs, _) => {
      return rhs
    }, tmp3, tmp4);
    tmp6 = ParseRule.Choice.keyword(Precedence.TypeKeywords._colon, tmp2, tmp5);
    return ParseRule.Choice.typeExpr(tmp, tmp1, tmp6);
  } 
  static get labelDecls() {
    let intro, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;
    intro = "label-decls: ";
    tmp = intro + "label-decl";
    tmp1 = ParseRule.Choice.end(Stack.Nil);
    tmp2 = intro + "semicolon";
    tmp3 = intro + "end";
    tmp4 = ParseRule.Choice.end(runtime.Unit);
    tmp5 = ParseRule.Choice.reference("label-decls", (more, _) => {
      return more
    }, tmp3, tmp4);
    tmp6 = ParseRule.Choice.keyword(Precedence.Keywords._semicolon, tmp2, tmp5);
    return ParseRule.Choice.reference("label-decl", Stack.Cons, tmp, tmp1, tmp6);
  } 
  static exceptionDefinition() {
    let intro, tmp, tmp1, tmp2;
    intro = "exception definition: ";
    tmp = intro + "keyword";
    tmp1 = ParseRule.Choice.end(runtime.Unit);
    tmp2 = ParseRule.Choice.reference("constr-decls", (decls, _) => {
      return Tree.Define(Tree.DefineKind.Exception, decls)
    }, "constructor declarations", tmp1);
    return ParseRule.Choice.keyword(Precedence.Keywords._exception, tmp, tmp2)
  } 
  static parse(tokens) {
    let parseKind, require, typeExprCont, term, parseRule, yeetSpaces, modCont, consume, termCont, typeExpr, mod, current, counter, tree, scrut, param0, param1, token, message, tmp, tmp1, tmp2, tmp3;
    yeetSpaces = function yeetSpaces() {
      let param01, param11, tail, tmp4, tmp5, tmp6, tmp7;
      tmp8: while (true) {
        if (current instanceof Stack.Cons.class) {
          param01 = current.head;
          param11 = current.tail;
          if (param01 instanceof Token.Space.class) {
            tail = param11;
            tmp4 = "skipped a space at " + counter;
            tmp5 = runtime.safeCall(Parser.tracer.print(tmp4));
            current = tail;
            tmp6 = counter + 1;
            counter = tmp6;
            tmp7 = runtime.Unit;
            continue tmp8;
          } else {
            tmp7 = runtime.Unit;
          }
        } else {
          tmp7 = runtime.Unit;
        }
        break;
      }
      return current
    };
    consume = function consume() {
      let param01, param11, head, tail, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9;
      if (current instanceof Stack.Cons.class) {
        param01 = current.head;
        param11 = current.tail;
        head = param01;
        tail = param11;
        tmp4 = Token.summary(head);
        tmp5 = "consumed `" + tmp4;
        tmp6 = tmp5 + "` at ";
        tmp7 = tmp6 + counter;
        tmp8 = runtime.safeCall(Parser.tracer.print(tmp7));
        current = tail;
        tmp9 = counter + 1;
        counter = tmp9;
        return runtime.Unit
      } else {
        return runtime.safeCall(Parser.tracer.print("consumed: EOF"))
      }
    };
    require = function require(result, expected) {
      let scrut1, param01, param11, actual, scrut2, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10;
      tmp4 = yeetSpaces();
      scrut1 = tmp4;
      if (scrut1 instanceof Stack.Cons.class) {
        param01 = scrut1.head;
        param11 = scrut1.tail;
        actual = param01;
        scrut2 = Token.same(expected, actual);
        if (scrut2 === true) {
          tmp5 = consume();
          return result
        } else {
          tmp6 = Token.summary(expected);
          tmp7 = Token.summary(actual);
          tmp8 = Predef.mkStr("Expected token ", tmp6, ", but found ", tmp7);
          return Tree.Error(result, tmp8)
        }
      } else if (scrut1 instanceof Stack.Nil.class) {
        tmp9 = Token.summary(expected);
        tmp10 = Predef.mkStr("Expected token ", tmp9, ", but found end of input");
        return Tree.Error(result, tmp10)
      } else {
        throw new globalThis.Error("match error");
      }
    };
    parseKind = function parseKind(kind1, prec) {
      let scrut1, param01, rule, tmp4, tmp5;
      if (kind1 === "type") {
        return typeExpr(prec, Precedence.TypeKeywords.all)
      } else if (kind1 === "term") {
        return term(prec, Precedence.Keywords.all)
      } else {
        scrut1 = runtime.safeCall(Parser.syntaxKinds.get(kind1));
        if (scrut1 instanceof Option.Some.class) {
          param01 = scrut1.value;
          rule = param01;
          return parseRule(prec, rule, Precedence.Keywords.all)
        } else {
          tmp4 = "Unknown syntax kind: \"" + kind1;
          tmp5 = tmp4 + "\"";
          throw globalThis.Error(tmp5);
        }
      }
    };
    parseRule = function parseRule(prec, rule, allKeywords) {
      let tmp4, tmp5, tmp6, tmp7;
      tmp4 = "parsing rule \"" + rule.name;
      tmp5 = tmp4 + "\" with precedence ";
      tmp6 = tmp5 + prec;
      tmp7 = () => {
        let scrut1, scrut2, param01, value, param02, param11, other, doTemp, doTemp1, doTemp2, scrut3, param03, value1, scrut4, param04, first4, first3, first2, first1, first0, kind1, process, outerPrec, innerPrec, rest, doTemp3, acc, doTemp4, scrut5, tree1, param05, param12, name1, doTemp5, doTemp6, scrut6, param06, keyword1, doTemp7, doTemp8, scrut7, doTemp9, doTemp10, doTemp11, scrut8, param07, value2, scrut9, param08, first41, first31, first21, first11, first01, kind2, process1, outerPrec1, innerPrec1, rest1, acc1, tree2, param09, rest2, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26, tmp27, tmp28, tmp29, tmp30, tmp31, tmp32, tmp33, tmp34, tmp35, tmp36, tmp37, tmp38, tmp39, tmp40, tmp41, tmp42, tmp43, tmp44, tmp45, tmp46, tmp47, tmp48, tmp49, tmp50, tmp51, tmp52, tmp53, tmp54, tmp55, tmp56, tmp57, tmp58, tmp59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66, tmp67, tmp68, tmp69, tmp70, tmp71, tmp72, tmp73, tmp74, tmp75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82, tmp83, tmp84, tmp85, tmp86, tmp87, tmp88, tmp89, tmp90, tmp91, tmp92, tmp93, tmp94, tmp95, tmp96, tmp97, tmp98, tmp99, tmp100, tmp101, tmp102, tmp103, tmp104, tmp105, tmp106, tmp107, tmp108, tmp109, tmp110, tmp111, tmp112, tmp113, tmp114, tmp115, tmp116;
        tmp8 = yeetSpaces();
        scrut1 = tmp8;
        if (scrut1 instanceof Stack.Cons.class) {
          param02 = scrut1.head;
          param11 = scrut1.tail;
          if (param02 instanceof Token.Identifier.class) {
            param05 = param02.name;
            param12 = param02.symbolic;
            name1 = param05;
            tmp9 = "found an identifier \"" + name1;
            tmp10 = tmp9 + "\"";
            doTemp5 = Parser.tracer.print(tmp10, 627);
            scrut6 = runtime.safeCall(allKeywords.get(name1));
            if (scrut6 instanceof Option.Some.class) {
              param06 = scrut6.value;
              keyword1 = param06;
              tmp11 = runtime.safeCall(keyword1.toString());
              doTemp7 = Parser.tracer.print(tmp11, 629);
              tmp12 = (caseScrut) => {
                let first12, first02, k, v, tmp117;
                if (globalThis.Array.isArray(caseScrut) && caseScrut.length === 2) {
                  first02 = caseScrut[0];
                  first12 = caseScrut[1];
                  k = first02;
                  v = first12;
                  tmp117 = "`" + k;
                  return tmp117 + "`"
                } else {
                  throw new globalThis.Error("match error");
                }
              };
              tmp13 = Iter.mapping(rule.keywordChoices, tmp12);
              tmp14 = Iter.joined(tmp13, ", ");
              doTemp8 = Parser.tracer.print("keyword choices: ", tmp14);
              scrut7 = runtime.safeCall(rule.keywordChoices.get(name1));
              if (scrut7 instanceof Option.Some.class) {
                param09 = scrut7.value;
                rest2 = param09;
                tmp15 = "found a rule starting with `" + name1;
                tmp16 = tmp15 + "`";
                tmp17 = Parser.tracer.print(tmp16, 635);
                tmp18 = "the rest of the rule: " + rest2.display;
                tmp19 = Parser.tracer.print(tmp18, 636);
                tmp20 = consume();
                return parseRule(0, rest2, allKeywords)
              } else if (scrut7 instanceof Option.None.class) {
                tmp21 = "no rule starting with `" + name1;
                tmp22 = tmp21 + "` was found";
                doTemp9 = Parser.tracer.print(tmp22, 640);
                tmp23 = "the left prec of `" + name1;
                tmp24 = tmp23 + "` is ";
                tmp25 = tmp24 + keyword1.leftPrec;
                doTemp10 = Parser.tracer.print(tmp25, 641);
                scrut9 = rule.exprChoice;
                if (scrut9 instanceof Option.Some.class) {
                  param08 = scrut9.value;
                  if (globalThis.Array.isArray(param08) && param08.length === 5) {
                    first01 = param08[0];
                    first11 = param08[1];
                    first21 = param08[2];
                    first31 = param08[3];
                    first41 = param08[4];
                    kind2 = first01;
                    process1 = first11;
                    outerPrec1 = first21;
                    innerPrec1 = first31;
                    rest1 = first41;
                    tmp26 = Parser.tracer.print("found an expression choice", 644);
                    tmp27 = parseKind(kind2, prec);
                    acc1 = tmp27;
                    tmp28 = parseRule(prec, rest1, allKeywords);
                    tree2 = tmp28;
                    return runtime.safeCall(process1(acc1, tree2))
                  } else {
                    tmp29 = "no exprChoice or the prec is less than " + prec;
                    doTemp11 = Parser.tracer.print(tmp29, 648);
                    scrut8 = rule.endChoice;
                    if (scrut8 instanceof Option.Some.class) {
                      param07 = scrut8.value;
                      value2 = param07;
                      tmp30 = Parser.tracer.print("found end choice", 650);
                      return value2
                    } else {
                      tmp31 = consume();
                      tmp32 = "unexpected keyword " + keyword1.name;
                      return Tree.error(tmp32)
                    }
                  }
                } else {
                  tmp33 = "no exprChoice or the prec is less than " + prec;
                  doTemp11 = Parser.tracer.print(tmp33, 648);
                  scrut8 = rule.endChoice;
                  if (scrut8 instanceof Option.Some.class) {
                    param07 = scrut8.value;
                    value2 = param07;
                    tmp34 = Parser.tracer.print("found end choice", 650);
                    return value2
                  } else {
                    tmp35 = consume();
                    tmp36 = "unexpected keyword " + keyword1.name;
                    return Tree.error(tmp36)
                  }
                }
              } else {
                tmp37 = "\"" + name1;
                tmp38 = tmp37 + "\" is not a keyword";
                doTemp6 = Parser.tracer.print(tmp38, 655);
                other = param02;
                tmp39 = "the current rule is " + rule.display;
                doTemp = runtime.safeCall(Parser.tracer.print(tmp39));
                scrut4 = rule.exprChoice;
                if (scrut4 instanceof Option.Some.class) {
                  param04 = scrut4.value;
                  if (globalThis.Array.isArray(param04) && param04.length === 5) {
                    first0 = param04[0];
                    first1 = param04[1];
                    first2 = param04[2];
                    first3 = param04[3];
                    first4 = param04[4];
                    kind1 = first0;
                    process = first1;
                    outerPrec = first2;
                    innerPrec = first3;
                    rest = first4;
                    tmp40 = "parse \"" + kind1;
                    tmp41 = tmp40 + "\" kind from ";
                    tmp42 = TokenHelpers.preview(current);
                    tmp43 = tmp41 + tmp42;
                    doTemp3 = Parser.tracer.print(tmp43, 660);
                    acc = parseKind(kind1, prec);
                    scrut5 = Tree.nonEmptyError(acc);
                    if (scrut5 === true) {
                      tmp44 = "the rest rule: " + rest.display;
                      tmp45 = Parser.tracer.print(tmp44, 663);
                      tmp46 = parseRule(prec, rest, allKeywords);
                      tree1 = tmp46;
                      tmp47 = Tree.summary(acc);
                      tmp48 = "acc: " + tmp47;
                      tmp49 = Parser.tracer.print(tmp48, 665);
                      tmp50 = Tree.summary(tree1);
                      tmp51 = "tree: " + tmp50;
                      tmp52 = Parser.tracer.print(tmp51, 666);
                      tmp53 = "tree AST is: " + tree1;
                      tmp54 = Parser.tracer.print(tmp53, 667);
                      return runtime.safeCall(process(acc, tree1))
                    } else {
                      doTemp4 = Parser.tracer.print("fallback to end choice", 669);
                      doTemp1 = Parser.tracer.print("no expression choice", 670);
                      scrut3 = rule.endChoice;
                      if (scrut3 instanceof Option.Some.class) {
                        param03 = scrut3.value;
                        value1 = param03;
                        tmp55 = Parser.tracer.print("found end choice", 672);
                        return value1
                      } else {
                        doTemp2 = Parser.tracer.print("no end choice", 674);
                        tmp56 = consume();
                        tmp57 = "unexpected token " + other;
                        return Tree.error(tmp57)
                      }
                    }
                  } else {
                    doTemp1 = Parser.tracer.print("no expression choice", 670);
                    scrut3 = rule.endChoice;
                    if (scrut3 instanceof Option.Some.class) {
                      param03 = scrut3.value;
                      value1 = param03;
                      tmp58 = Parser.tracer.print("found end choice", 672);
                      return value1
                    } else {
                      doTemp2 = Parser.tracer.print("no end choice", 674);
                      tmp59 = consume();
                      tmp60 = "unexpected token " + other;
                      return Tree.error(tmp60)
                    }
                  }
                } else {
                  doTemp1 = Parser.tracer.print("no expression choice", 670);
                  scrut3 = rule.endChoice;
                  if (scrut3 instanceof Option.Some.class) {
                    param03 = scrut3.value;
                    value1 = param03;
                    tmp61 = Parser.tracer.print("found end choice", 672);
                    return value1
                  } else {
                    doTemp2 = Parser.tracer.print("no end choice", 674);
                    tmp62 = consume();
                    tmp63 = "unexpected token " + other;
                    return Tree.error(tmp63)
                  }
                }
              }
            } else {
              tmp64 = "\"" + name1;
              tmp65 = tmp64 + "\" is not a keyword";
              doTemp6 = Parser.tracer.print(tmp65, 655);
              other = param02;
              tmp66 = "the current rule is " + rule.display;
              doTemp = runtime.safeCall(Parser.tracer.print(tmp66));
              scrut4 = rule.exprChoice;
              if (scrut4 instanceof Option.Some.class) {
                param04 = scrut4.value;
                if (globalThis.Array.isArray(param04) && param04.length === 5) {
                  first0 = param04[0];
                  first1 = param04[1];
                  first2 = param04[2];
                  first3 = param04[3];
                  first4 = param04[4];
                  kind1 = first0;
                  process = first1;
                  outerPrec = first2;
                  innerPrec = first3;
                  rest = first4;
                  tmp67 = "parse \"" + kind1;
                  tmp68 = tmp67 + "\" kind from ";
                  tmp69 = TokenHelpers.preview(current);
                  tmp70 = tmp68 + tmp69;
                  doTemp3 = Parser.tracer.print(tmp70, 660);
                  acc = parseKind(kind1, prec);
                  scrut5 = Tree.nonEmptyError(acc);
                  if (scrut5 === true) {
                    tmp71 = "the rest rule: " + rest.display;
                    tmp72 = Parser.tracer.print(tmp71, 663);
                    tmp73 = parseRule(prec, rest, allKeywords);
                    tree1 = tmp73;
                    tmp74 = Tree.summary(acc);
                    tmp75 = "acc: " + tmp74;
                    tmp76 = Parser.tracer.print(tmp75, 665);
                    tmp77 = Tree.summary(tree1);
                    tmp78 = "tree: " + tmp77;
                    tmp79 = Parser.tracer.print(tmp78, 666);
                    tmp80 = "tree AST is: " + tree1;
                    tmp81 = Parser.tracer.print(tmp80, 667);
                    return runtime.safeCall(process(acc, tree1))
                  } else {
                    doTemp4 = Parser.tracer.print("fallback to end choice", 669);
                    doTemp1 = Parser.tracer.print("no expression choice", 670);
                    scrut3 = rule.endChoice;
                    if (scrut3 instanceof Option.Some.class) {
                      param03 = scrut3.value;
                      value1 = param03;
                      tmp82 = Parser.tracer.print("found end choice", 672);
                      return value1
                    } else {
                      doTemp2 = Parser.tracer.print("no end choice", 674);
                      tmp83 = consume();
                      tmp84 = "unexpected token " + other;
                      return Tree.error(tmp84)
                    }
                  }
                } else {
                  doTemp1 = Parser.tracer.print("no expression choice", 670);
                  scrut3 = rule.endChoice;
                  if (scrut3 instanceof Option.Some.class) {
                    param03 = scrut3.value;
                    value1 = param03;
                    tmp85 = Parser.tracer.print("found end choice", 672);
                    return value1
                  } else {
                    doTemp2 = Parser.tracer.print("no end choice", 674);
                    tmp86 = consume();
                    tmp87 = "unexpected token " + other;
                    return Tree.error(tmp87)
                  }
                }
              } else {
                doTemp1 = Parser.tracer.print("no expression choice", 670);
                scrut3 = rule.endChoice;
                if (scrut3 instanceof Option.Some.class) {
                  param03 = scrut3.value;
                  value1 = param03;
                  tmp88 = Parser.tracer.print("found end choice", 672);
                  return value1
                } else {
                  doTemp2 = Parser.tracer.print("no end choice", 674);
                  tmp89 = consume();
                  tmp90 = "unexpected token " + other;
                  return Tree.error(tmp90)
                }
              }
            }
          } else {
            other = param02;
            tmp91 = "the current rule is " + rule.display;
            doTemp = runtime.safeCall(Parser.tracer.print(tmp91));
            scrut4 = rule.exprChoice;
            if (scrut4 instanceof Option.Some.class) {
              param04 = scrut4.value;
              if (globalThis.Array.isArray(param04) && param04.length === 5) {
                first0 = param04[0];
                first1 = param04[1];
                first2 = param04[2];
                first3 = param04[3];
                first4 = param04[4];
                kind1 = first0;
                process = first1;
                outerPrec = first2;
                innerPrec = first3;
                rest = first4;
                tmp92 = "parse \"" + kind1;
                tmp93 = tmp92 + "\" kind from ";
                tmp94 = TokenHelpers.preview(current);
                tmp95 = tmp93 + tmp94;
                doTemp3 = Parser.tracer.print(tmp95, 660);
                acc = parseKind(kind1, prec);
                scrut5 = Tree.nonEmptyError(acc);
                if (scrut5 === true) {
                  tmp96 = "the rest rule: " + rest.display;
                  tmp97 = Parser.tracer.print(tmp96, 663);
                  tmp98 = parseRule(prec, rest, allKeywords);
                  tree1 = tmp98;
                  tmp99 = Tree.summary(acc);
                  tmp100 = "acc: " + tmp99;
                  tmp101 = Parser.tracer.print(tmp100, 665);
                  tmp102 = Tree.summary(tree1);
                  tmp103 = "tree: " + tmp102;
                  tmp104 = Parser.tracer.print(tmp103, 666);
                  tmp105 = "tree AST is: " + tree1;
                  tmp106 = Parser.tracer.print(tmp105, 667);
                  return runtime.safeCall(process(acc, tree1))
                } else {
                  doTemp4 = Parser.tracer.print("fallback to end choice", 669);
                  doTemp1 = Parser.tracer.print("no expression choice", 670);
                  scrut3 = rule.endChoice;
                  if (scrut3 instanceof Option.Some.class) {
                    param03 = scrut3.value;
                    value1 = param03;
                    tmp107 = Parser.tracer.print("found end choice", 672);
                    return value1
                  } else {
                    doTemp2 = Parser.tracer.print("no end choice", 674);
                    tmp108 = consume();
                    tmp109 = "unexpected token " + other;
                    return Tree.error(tmp109)
                  }
                }
              } else {
                doTemp1 = Parser.tracer.print("no expression choice", 670);
                scrut3 = rule.endChoice;
                if (scrut3 instanceof Option.Some.class) {
                  param03 = scrut3.value;
                  value1 = param03;
                  tmp110 = Parser.tracer.print("found end choice", 672);
                  return value1
                } else {
                  doTemp2 = Parser.tracer.print("no end choice", 674);
                  tmp111 = consume();
                  tmp112 = "unexpected token " + other;
                  return Tree.error(tmp112)
                }
              }
            } else {
              doTemp1 = Parser.tracer.print("no expression choice", 670);
              scrut3 = rule.endChoice;
              if (scrut3 instanceof Option.Some.class) {
                param03 = scrut3.value;
                value1 = param03;
                tmp113 = Parser.tracer.print("found end choice", 672);
                return value1
              } else {
                doTemp2 = Parser.tracer.print("no end choice", 674);
                tmp114 = consume();
                tmp115 = "unexpected token " + other;
                return Tree.error(tmp115)
              }
            }
          }
        } else if (scrut1 instanceof Stack.Nil.class) {
          scrut2 = rule.endChoice;
          if (scrut2 instanceof Option.Some.class) {
            param01 = scrut2.value;
            value = param01;
            return value
          } else if (scrut2 instanceof Option.None.class) {
            tmp116 = Parser.tracer.print("no end choice but found the end of input", 681);
            return Tree.error("unexpected end of input")
          } else {
            throw new globalThis.Error("match error");
          }
        } else {
          throw new globalThis.Error("match error");
        }
      };
      return runtime.safeCall(Parser.tracer.trace(tmp6, (result) => {
        let tmp8, tmp9, tmp10;
        tmp8 = "parsed rule \"" + rule.name;
        tmp9 = tmp8 + "\": ";
        tmp10 = Tree.summary(result);
        return tmp9 + tmp10
      }, tmp7))
    };
    term = function term(prec) {
      let tmp4, tmp5, tmp6, tmp7, tmp8;
      tmp4 = "term <<< " + prec;
      tmp5 = tmp4 + " ";
      tmp6 = TokenHelpers.preview(current);
      tmp7 = tmp5 + tmp6;
      tmp8 = () => {
        let scrut1, param01, param11, token1, param02, param12, kind1, literal, param03, param13, name1, symbolic, scrut2, param04, keyword1, scrut3, param05, rule, scrut4, acc, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, tmp20, tmp21, tmp22;
        tmp9 = yeetSpaces();
        scrut1 = tmp9;
        if (scrut1 instanceof Stack.Cons.class) {
          param01 = scrut1.head;
          param11 = scrut1.tail;
          if (param01 instanceof Token.Identifier.class) {
            param03 = param01.name;
            param13 = param01.symbolic;
            name1 = param03;
            symbolic = param13;
            scrut2 = runtime.safeCall(Precedence.Keywords.all.get(name1));
            if (scrut2 instanceof Option.Some.class) {
              param04 = scrut2.value;
              keyword1 = param04;
              scrut3 = runtime.safeCall(Parser.termRule.keywordChoices.get(name1));
              if (scrut3 instanceof Option.Some.class) {
                param05 = scrut3.value;
                rule = param05;
                scrut4 = keyword1.leftPrecOrMin > prec;
                if (scrut4 === true) {
                  tmp10 = consume();
                  tmp11 = parseRule(keyword1.rightPrecOrMax, rule, Precedence.Keywords.all);
                  acc = tmp11;
                  return termCont(acc, prec)
                } else {
                  tmp12 = "the left precedence is less" + name1;
                  tmp13 = Parser.tracer.print(tmp12, 697);
                  return Tree.empty
                }
              } else if (scrut3 instanceof Option.None.class) {
                tmp14 = "no rule starting with " + name1;
                tmp15 = Parser.tracer.print(tmp14, 700);
                return Tree.empty
              } else {
                token1 = param01;
                tmp16 = "unrecognized token: " + token1;
                return Tree.error(tmp16)
              }
            } else if (scrut2 instanceof Option.None.class) {
              tmp17 = consume();
              tmp18 = Tree.Ident(name1, symbolic);
              return termCont(tmp18, prec)
            } else {
              token1 = param01;
              tmp19 = "unrecognized token: " + token1;
              return Tree.error(tmp19)
            }
          } else if (param01 instanceof Token.Literal.class) {
            param02 = param01.kind;
            param12 = param01.literal;
            kind1 = param02;
            literal = param12;
            tmp20 = consume();
            tmp21 = Tree.Literal(kind1, literal);
            return termCont(tmp21, prec)
          } else {
            token1 = param01;
            tmp22 = "unrecognized token: " + token1;
            return Tree.error(tmp22)
          }
        } else if (scrut1 instanceof Stack.Nil.class) {
          return Tree.error("unexpected end of input")
        } else {
          throw new globalThis.Error("match error");
        }
      };
      return runtime.safeCall(Parser.tracer.trace(tmp7, (result) => {
        let tmp9;
        tmp9 = Tree.summary(result);
        return "term >>> " + tmp9
      }, tmp8))
    };
    termCont = function termCont(acc, prec) {
      let scrut1, doTemp, param01, param11, token1, scrut2, param02, first4, first3, first2, first1, first0, kind1, process, outerPrec, innerPrec, rest, doTemp1, outerPrec$_, doTemp2, scrut3, scrut4, rhs, param03, param12, name1, scrut5, doTemp3, scrut6, first11, first01, leftPrec, rightPrec, doTemp4, scrut7, op, rhs1, name2, scrut8, param04, keyword1, doTemp5, doTemp6, scrut9, param05, rule, doTemp7, scrut10, scrut11, param06, first41, first31, first21, first12, first02, kind2, process1, outerPrec1, innerPrec1, rest1, rhs2, acc$_, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26, tmp27, tmp28, tmp29, tmp30, tmp31, tmp32, tmp33, tmp34, tmp35, tmp36, tmp37, tmp38, tmp39, tmp40, tmp41, tmp42, tmp43, tmp44, tmp45, tmp46, tmp47, tmp48, tmp49, tmp50, tmp51, tmp52, tmp53, tmp54, tmp55, tmp56, tmp57, tmp58, tmp59, tmp60, tmp61, tmp62, tmp63, tmp64, tmp65, tmp66, tmp67, tmp68, tmp69, tmp70, tmp71, tmp72, tmp73, tmp74, tmp75, tmp76, tmp77, tmp78, tmp79, tmp80, tmp81, tmp82, tmp83, tmp84, tmp85, tmp86, tmp87, tmp88, tmp89, tmp90, tmp91, tmp92, tmp93, tmp94, tmp95, tmp96, tmp97, tmp98, tmp99, tmp100, tmp101, tmp102, tmp103, tmp104, tmp105, tmp106, tmp107, tmp108, tmp109, tmp110, tmp111, tmp112, tmp113, tmp114, tmp115, tmp116, tmp117, tmp118, tmp119, tmp120, tmp121, tmp122, tmp123, tmp124, tmp125, tmp126, tmp127, tmp128, tmp129, tmp130, tmp131, tmp132, tmp133, tmp134, tmp135, tmp136, tmp137, tmp138, tmp139, tmp140, tmp141, tmp142, tmp143, tmp144, tmp145, tmp146, tmp147, tmp148, tmp149, tmp150, tmp151, tmp152, tmp153, tmp154, tmp155, tmp156, tmp157, tmp158, tmp159, tmp160, tmp161, tmp162, tmp163, tmp164, tmp165, tmp166, tmp167, tmp168, tmp169, tmp170, tmp171, tmp172, tmp173, tmp174, tmp175, tmp176, tmp177, tmp178, tmp179, tmp180, tmp181, tmp182, tmp183, tmp184, tmp185, tmp186, tmp187, tmp188, tmp189, tmp190, tmp191, tmp192, tmp193, tmp194, tmp195, tmp196, tmp197, tmp198, tmp199, tmp200, tmp201, tmp202, tmp203, tmp204, tmp205, tmp206, tmp207, tmp208, tmp209, tmp210, tmp211, tmp212, tmp213, tmp214, tmp215, tmp216, tmp217, tmp218, tmp219, tmp220, tmp221, tmp222, tmp223, tmp224, tmp225, tmp226, tmp227, tmp228, tmp229, tmp230, tmp231, tmp232, tmp233, tmp234, tmp235, tmp236, tmp237, tmp238, tmp239, tmp240, tmp241, tmp242, tmp243, tmp244, tmp245, tmp246, tmp247, tmp248, tmp249, tmp250, tmp251, tmp252, tmp253, tmp254;
      tmp4 = yeetSpaces();
      scrut1 = tmp4;
      tmp5 = ">>> termCont " + prec;
      tmp6 = tmp5 + " ";
      tmp7 = Tree.summary(acc);
      tmp8 = tmp6 + tmp7;
      tmp9 = tmp8 + " <<<";
      doTemp = Parser.tracer.print(tmp9, 712);
      if (scrut1 instanceof Stack.Cons.class) {
        param01 = scrut1.head;
        param11 = scrut1.tail;
        if (param01 instanceof Token.Identifier.class) {
          param03 = param01.name;
          param12 = param01.symbolic;
          name2 = param03;
          scrut8 = runtime.safeCall(Precedence.Keywords.all.get(name2));
          if (scrut8 instanceof Option.Some.class) {
            param04 = scrut8.value;
            keyword1 = param04;
            tmp10 = "found a keyword: " + name2;
            doTemp5 = Parser.tracer.print(tmp10, 715);
            scrut9 = runtime.safeCall(Parser.termInfixRule.keywordChoices.get(name2));
            if (scrut9 instanceof Option.Some.class) {
              param05 = scrut9.value;
              rule = param05;
              tmp11 = "the keyword is found in infix rules" + name2;
              doTemp7 = Parser.tracer.print(tmp11, 719);
              scrut10 = keyword1.leftPrecOrMin > prec;
              if (scrut10 === true) {
                scrut11 = rule.exprChoice;
                if (scrut11 instanceof Option.Some.class) {
                  param06 = scrut11.value;
                  if (globalThis.Array.isArray(param06) && param06.length === 5) {
                    first02 = param06[0];
                    first12 = param06[1];
                    first21 = param06[2];
                    first31 = param06[3];
                    first41 = param06[4];
                    kind2 = first02;
                    process1 = first12;
                    outerPrec1 = first21;
                    innerPrec1 = first31;
                    rest1 = first41;
                    tmp12 = consume();
                    tmp13 = parseKind(kind2, keyword1.rightPrecOrMin);
                    rhs2 = tmp13;
                    tmp14 = runtime.safeCall(process1(rhs2, runtime.Unit));
                    tmp15 = runtime.safeCall(tmp14(acc));
                    acc$_ = tmp15;
                    return termCont(acc$_, prec)
                  } else {
                    tmp16 = "keyword `" + name2;
                    tmp17 = tmp16 + "` does not have infix rules";
                    doTemp6 = Parser.tracer.print(tmp17, 728);
                    name1 = param03;
                    if (param12 === true) {
                      scrut5 = runtime.safeCall(Precedence.Keywords.all.get(name1));
                      if (scrut5 instanceof Option.None.class) {
                        tmp18 = "found an operator \"" + name1;
                        tmp19 = tmp18 + "\"";
                        doTemp3 = Parser.tracer.print(tmp19, 730);
                        scrut6 = Precedence.opPrec(name1);
                        if (globalThis.Array.isArray(scrut6) && scrut6.length === 2) {
                          first01 = scrut6[0];
                          first11 = scrut6[1];
                          leftPrec = first01;
                          rightPrec = first11;
                          tmp20 = "leftPrec = " + leftPrec;
                          tmp21 = tmp20 + "; rightPrec = ";
                          tmp22 = tmp21 + rightPrec;
                          doTemp4 = Parser.tracer.print(tmp22, 732);
                          scrut7 = leftPrec > prec;
                          if (scrut7 === true) {
                            tmp23 = consume();
                            tmp24 = Tree.Ident(name1, true);
                            op = tmp24;
                            tmp25 = term(rightPrec);
                            rhs1 = tmp25;
                            tmp26 = Stack.Cons(rhs1, Stack.Nil);
                            tmp27 = Stack.Cons(acc, tmp26);
                            tmp28 = Tree.App(op, tmp27);
                            return termCont(tmp28, prec)
                          } else {
                            return acc
                          }
                        } else {
                          token1 = param01;
                          scrut2 = Parser.termInfixRule.exprChoice;
                          if (scrut2 instanceof Option.Some.class) {
                            param02 = scrut2.value;
                            if (globalThis.Array.isArray(param02) && param02.length === 5) {
                              first0 = param02[0];
                              first1 = param02[1];
                              first2 = param02[2];
                              first3 = param02[3];
                              first4 = param02[4];
                              kind1 = first0;
                              process = first1;
                              outerPrec = first2;
                              innerPrec = first3;
                              rest = first4;
                              tmp29 = "found an exprChoice with outerPrec = " + outerPrec;
                              doTemp1 = Parser.tracer.print(tmp29, 742);
                              outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                              scrut3 = outerPrec$_ > prec;
                              if (scrut3 === true) {
                                tmp30 = Option.getOrElse(innerPrec, outerPrec$_);
                                scrut4 = parseKind(kind1, tmp30);
                                if (scrut4 instanceof Tree.Empty.class) {
                                  tmp31 = Parser.tracer.print("nothing was parsed", 747);
                                  return acc
                                } else {
                                  rhs = scrut4;
                                  tmp32 = Tree.summary(rhs);
                                  tmp33 = "parsed " + tmp32;
                                  tmp34 = Parser.tracer.print(tmp33, 750);
                                  tmp35 = runtime.safeCall(process(rhs, runtime.Unit));
                                  tmp36 = runtime.safeCall(tmp35(acc));
                                  return termCont(tmp36, prec)
                                }
                              } else {
                                tmp37 = "the outer precedence is less than " + prec;
                                doTemp2 = Parser.tracer.print(tmp37, 752);
                                return acc
                              }
                            } else {
                              throw new globalThis.Error("match error");
                            }
                          } else if (scrut2 instanceof Option.None.class) {
                            tmp38 = "cannot consume " + token1;
                            tmp39 = Parser.tracer.print(tmp38, 755);
                            return acc
                          } else {
                            throw new globalThis.Error("match error");
                          }
                        }
                      } else {
                        token1 = param01;
                        scrut2 = Parser.termInfixRule.exprChoice;
                        if (scrut2 instanceof Option.Some.class) {
                          param02 = scrut2.value;
                          if (globalThis.Array.isArray(param02) && param02.length === 5) {
                            first0 = param02[0];
                            first1 = param02[1];
                            first2 = param02[2];
                            first3 = param02[3];
                            first4 = param02[4];
                            kind1 = first0;
                            process = first1;
                            outerPrec = first2;
                            innerPrec = first3;
                            rest = first4;
                            tmp40 = "found an exprChoice with outerPrec = " + outerPrec;
                            doTemp1 = Parser.tracer.print(tmp40, 742);
                            outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                            scrut3 = outerPrec$_ > prec;
                            if (scrut3 === true) {
                              tmp41 = Option.getOrElse(innerPrec, outerPrec$_);
                              scrut4 = parseKind(kind1, tmp41);
                              if (scrut4 instanceof Tree.Empty.class) {
                                tmp42 = Parser.tracer.print("nothing was parsed", 747);
                                return acc
                              } else {
                                rhs = scrut4;
                                tmp43 = Tree.summary(rhs);
                                tmp44 = "parsed " + tmp43;
                                tmp45 = Parser.tracer.print(tmp44, 750);
                                tmp46 = runtime.safeCall(process(rhs, runtime.Unit));
                                tmp47 = runtime.safeCall(tmp46(acc));
                                return termCont(tmp47, prec)
                              }
                            } else {
                              tmp48 = "the outer precedence is less than " + prec;
                              doTemp2 = Parser.tracer.print(tmp48, 752);
                              return acc
                            }
                          } else {
                            throw new globalThis.Error("match error");
                          }
                        } else if (scrut2 instanceof Option.None.class) {
                          tmp49 = "cannot consume " + token1;
                          tmp50 = Parser.tracer.print(tmp49, 755);
                          return acc
                        } else {
                          throw new globalThis.Error("match error");
                        }
                      }
                    } else {
                      token1 = param01;
                      scrut2 = Parser.termInfixRule.exprChoice;
                      if (scrut2 instanceof Option.Some.class) {
                        param02 = scrut2.value;
                        if (globalThis.Array.isArray(param02) && param02.length === 5) {
                          first0 = param02[0];
                          first1 = param02[1];
                          first2 = param02[2];
                          first3 = param02[3];
                          first4 = param02[4];
                          kind1 = first0;
                          process = first1;
                          outerPrec = first2;
                          innerPrec = first3;
                          rest = first4;
                          tmp51 = "found an exprChoice with outerPrec = " + outerPrec;
                          doTemp1 = Parser.tracer.print(tmp51, 742);
                          outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                          scrut3 = outerPrec$_ > prec;
                          if (scrut3 === true) {
                            tmp52 = Option.getOrElse(innerPrec, outerPrec$_);
                            scrut4 = parseKind(kind1, tmp52);
                            if (scrut4 instanceof Tree.Empty.class) {
                              tmp53 = Parser.tracer.print("nothing was parsed", 747);
                              return acc
                            } else {
                              rhs = scrut4;
                              tmp54 = Tree.summary(rhs);
                              tmp55 = "parsed " + tmp54;
                              tmp56 = Parser.tracer.print(tmp55, 750);
                              tmp57 = runtime.safeCall(process(rhs, runtime.Unit));
                              tmp58 = runtime.safeCall(tmp57(acc));
                              return termCont(tmp58, prec)
                            }
                          } else {
                            tmp59 = "the outer precedence is less than " + prec;
                            doTemp2 = Parser.tracer.print(tmp59, 752);
                            return acc
                          }
                        } else {
                          throw new globalThis.Error("match error");
                        }
                      } else if (scrut2 instanceof Option.None.class) {
                        tmp60 = "cannot consume " + token1;
                        tmp61 = Parser.tracer.print(tmp60, 755);
                        return acc
                      } else {
                        throw new globalThis.Error("match error");
                      }
                    }
                  }
                } else if (scrut11 instanceof Option.None.class) {
                  return acc
                } else {
                  tmp62 = "keyword `" + name2;
                  tmp63 = tmp62 + "` does not have infix rules";
                  doTemp6 = Parser.tracer.print(tmp63, 728);
                  name1 = param03;
                  if (param12 === true) {
                    scrut5 = runtime.safeCall(Precedence.Keywords.all.get(name1));
                    if (scrut5 instanceof Option.None.class) {
                      tmp64 = "found an operator \"" + name1;
                      tmp65 = tmp64 + "\"";
                      doTemp3 = Parser.tracer.print(tmp65, 730);
                      scrut6 = Precedence.opPrec(name1);
                      if (globalThis.Array.isArray(scrut6) && scrut6.length === 2) {
                        first01 = scrut6[0];
                        first11 = scrut6[1];
                        leftPrec = first01;
                        rightPrec = first11;
                        tmp66 = "leftPrec = " + leftPrec;
                        tmp67 = tmp66 + "; rightPrec = ";
                        tmp68 = tmp67 + rightPrec;
                        doTemp4 = Parser.tracer.print(tmp68, 732);
                        scrut7 = leftPrec > prec;
                        if (scrut7 === true) {
                          tmp69 = consume();
                          tmp70 = Tree.Ident(name1, true);
                          op = tmp70;
                          tmp71 = term(rightPrec);
                          rhs1 = tmp71;
                          tmp72 = Stack.Cons(rhs1, Stack.Nil);
                          tmp73 = Stack.Cons(acc, tmp72);
                          tmp74 = Tree.App(op, tmp73);
                          return termCont(tmp74, prec)
                        } else {
                          return acc
                        }
                      } else {
                        token1 = param01;
                        scrut2 = Parser.termInfixRule.exprChoice;
                        if (scrut2 instanceof Option.Some.class) {
                          param02 = scrut2.value;
                          if (globalThis.Array.isArray(param02) && param02.length === 5) {
                            first0 = param02[0];
                            first1 = param02[1];
                            first2 = param02[2];
                            first3 = param02[3];
                            first4 = param02[4];
                            kind1 = first0;
                            process = first1;
                            outerPrec = first2;
                            innerPrec = first3;
                            rest = first4;
                            tmp75 = "found an exprChoice with outerPrec = " + outerPrec;
                            doTemp1 = Parser.tracer.print(tmp75, 742);
                            outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                            scrut3 = outerPrec$_ > prec;
                            if (scrut3 === true) {
                              tmp76 = Option.getOrElse(innerPrec, outerPrec$_);
                              scrut4 = parseKind(kind1, tmp76);
                              if (scrut4 instanceof Tree.Empty.class) {
                                tmp77 = Parser.tracer.print("nothing was parsed", 747);
                                return acc
                              } else {
                                rhs = scrut4;
                                tmp78 = Tree.summary(rhs);
                                tmp79 = "parsed " + tmp78;
                                tmp80 = Parser.tracer.print(tmp79, 750);
                                tmp81 = runtime.safeCall(process(rhs, runtime.Unit));
                                tmp82 = runtime.safeCall(tmp81(acc));
                                return termCont(tmp82, prec)
                              }
                            } else {
                              tmp83 = "the outer precedence is less than " + prec;
                              doTemp2 = Parser.tracer.print(tmp83, 752);
                              return acc
                            }
                          } else {
                            throw new globalThis.Error("match error");
                          }
                        } else if (scrut2 instanceof Option.None.class) {
                          tmp84 = "cannot consume " + token1;
                          tmp85 = Parser.tracer.print(tmp84, 755);
                          return acc
                        } else {
                          throw new globalThis.Error("match error");
                        }
                      }
                    } else {
                      token1 = param01;
                      scrut2 = Parser.termInfixRule.exprChoice;
                      if (scrut2 instanceof Option.Some.class) {
                        param02 = scrut2.value;
                        if (globalThis.Array.isArray(param02) && param02.length === 5) {
                          first0 = param02[0];
                          first1 = param02[1];
                          first2 = param02[2];
                          first3 = param02[3];
                          first4 = param02[4];
                          kind1 = first0;
                          process = first1;
                          outerPrec = first2;
                          innerPrec = first3;
                          rest = first4;
                          tmp86 = "found an exprChoice with outerPrec = " + outerPrec;
                          doTemp1 = Parser.tracer.print(tmp86, 742);
                          outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                          scrut3 = outerPrec$_ > prec;
                          if (scrut3 === true) {
                            tmp87 = Option.getOrElse(innerPrec, outerPrec$_);
                            scrut4 = parseKind(kind1, tmp87);
                            if (scrut4 instanceof Tree.Empty.class) {
                              tmp88 = Parser.tracer.print("nothing was parsed", 747);
                              return acc
                            } else {
                              rhs = scrut4;
                              tmp89 = Tree.summary(rhs);
                              tmp90 = "parsed " + tmp89;
                              tmp91 = Parser.tracer.print(tmp90, 750);
                              tmp92 = runtime.safeCall(process(rhs, runtime.Unit));
                              tmp93 = runtime.safeCall(tmp92(acc));
                              return termCont(tmp93, prec)
                            }
                          } else {
                            tmp94 = "the outer precedence is less than " + prec;
                            doTemp2 = Parser.tracer.print(tmp94, 752);
                            return acc
                          }
                        } else {
                          throw new globalThis.Error("match error");
                        }
                      } else if (scrut2 instanceof Option.None.class) {
                        tmp95 = "cannot consume " + token1;
                        tmp96 = Parser.tracer.print(tmp95, 755);
                        return acc
                      } else {
                        throw new globalThis.Error("match error");
                      }
                    }
                  } else {
                    token1 = param01;
                    scrut2 = Parser.termInfixRule.exprChoice;
                    if (scrut2 instanceof Option.Some.class) {
                      param02 = scrut2.value;
                      if (globalThis.Array.isArray(param02) && param02.length === 5) {
                        first0 = param02[0];
                        first1 = param02[1];
                        first2 = param02[2];
                        first3 = param02[3];
                        first4 = param02[4];
                        kind1 = first0;
                        process = first1;
                        outerPrec = first2;
                        innerPrec = first3;
                        rest = first4;
                        tmp97 = "found an exprChoice with outerPrec = " + outerPrec;
                        doTemp1 = Parser.tracer.print(tmp97, 742);
                        outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                        scrut3 = outerPrec$_ > prec;
                        if (scrut3 === true) {
                          tmp98 = Option.getOrElse(innerPrec, outerPrec$_);
                          scrut4 = parseKind(kind1, tmp98);
                          if (scrut4 instanceof Tree.Empty.class) {
                            tmp99 = Parser.tracer.print("nothing was parsed", 747);
                            return acc
                          } else {
                            rhs = scrut4;
                            tmp100 = Tree.summary(rhs);
                            tmp101 = "parsed " + tmp100;
                            tmp102 = Parser.tracer.print(tmp101, 750);
                            tmp103 = runtime.safeCall(process(rhs, runtime.Unit));
                            tmp104 = runtime.safeCall(tmp103(acc));
                            return termCont(tmp104, prec)
                          }
                        } else {
                          tmp105 = "the outer precedence is less than " + prec;
                          doTemp2 = Parser.tracer.print(tmp105, 752);
                          return acc
                        }
                      } else {
                        throw new globalThis.Error("match error");
                      }
                    } else if (scrut2 instanceof Option.None.class) {
                      tmp106 = "cannot consume " + token1;
                      tmp107 = Parser.tracer.print(tmp106, 755);
                      return acc
                    } else {
                      throw new globalThis.Error("match error");
                    }
                  }
                }
              } else {
                tmp108 = "keyword `" + name2;
                tmp109 = tmp108 + "` does not have infix rules";
                doTemp6 = Parser.tracer.print(tmp109, 728);
                name1 = param03;
                if (param12 === true) {
                  scrut5 = runtime.safeCall(Precedence.Keywords.all.get(name1));
                  if (scrut5 instanceof Option.None.class) {
                    tmp110 = "found an operator \"" + name1;
                    tmp111 = tmp110 + "\"";
                    doTemp3 = Parser.tracer.print(tmp111, 730);
                    scrut6 = Precedence.opPrec(name1);
                    if (globalThis.Array.isArray(scrut6) && scrut6.length === 2) {
                      first01 = scrut6[0];
                      first11 = scrut6[1];
                      leftPrec = first01;
                      rightPrec = first11;
                      tmp112 = "leftPrec = " + leftPrec;
                      tmp113 = tmp112 + "; rightPrec = ";
                      tmp114 = tmp113 + rightPrec;
                      doTemp4 = Parser.tracer.print(tmp114, 732);
                      scrut7 = leftPrec > prec;
                      if (scrut7 === true) {
                        tmp115 = consume();
                        tmp116 = Tree.Ident(name1, true);
                        op = tmp116;
                        tmp117 = term(rightPrec);
                        rhs1 = tmp117;
                        tmp118 = Stack.Cons(rhs1, Stack.Nil);
                        tmp119 = Stack.Cons(acc, tmp118);
                        tmp120 = Tree.App(op, tmp119);
                        return termCont(tmp120, prec)
                      } else {
                        return acc
                      }
                    } else {
                      token1 = param01;
                      scrut2 = Parser.termInfixRule.exprChoice;
                      if (scrut2 instanceof Option.Some.class) {
                        param02 = scrut2.value;
                        if (globalThis.Array.isArray(param02) && param02.length === 5) {
                          first0 = param02[0];
                          first1 = param02[1];
                          first2 = param02[2];
                          first3 = param02[3];
                          first4 = param02[4];
                          kind1 = first0;
                          process = first1;
                          outerPrec = first2;
                          innerPrec = first3;
                          rest = first4;
                          tmp121 = "found an exprChoice with outerPrec = " + outerPrec;
                          doTemp1 = Parser.tracer.print(tmp121, 742);
                          outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                          scrut3 = outerPrec$_ > prec;
                          if (scrut3 === true) {
                            tmp122 = Option.getOrElse(innerPrec, outerPrec$_);
                            scrut4 = parseKind(kind1, tmp122);
                            if (scrut4 instanceof Tree.Empty.class) {
                              tmp123 = Parser.tracer.print("nothing was parsed", 747);
                              return acc
                            } else {
                              rhs = scrut4;
                              tmp124 = Tree.summary(rhs);
                              tmp125 = "parsed " + tmp124;
                              tmp126 = Parser.tracer.print(tmp125, 750);
                              tmp127 = runtime.safeCall(process(rhs, runtime.Unit));
                              tmp128 = runtime.safeCall(tmp127(acc));
                              return termCont(tmp128, prec)
                            }
                          } else {
                            tmp129 = "the outer precedence is less than " + prec;
                            doTemp2 = Parser.tracer.print(tmp129, 752);
                            return acc
                          }
                        } else {
                          throw new globalThis.Error("match error");
                        }
                      } else if (scrut2 instanceof Option.None.class) {
                        tmp130 = "cannot consume " + token1;
                        tmp131 = Parser.tracer.print(tmp130, 755);
                        return acc
                      } else {
                        throw new globalThis.Error("match error");
                      }
                    }
                  } else {
                    token1 = param01;
                    scrut2 = Parser.termInfixRule.exprChoice;
                    if (scrut2 instanceof Option.Some.class) {
                      param02 = scrut2.value;
                      if (globalThis.Array.isArray(param02) && param02.length === 5) {
                        first0 = param02[0];
                        first1 = param02[1];
                        first2 = param02[2];
                        first3 = param02[3];
                        first4 = param02[4];
                        kind1 = first0;
                        process = first1;
                        outerPrec = first2;
                        innerPrec = first3;
                        rest = first4;
                        tmp132 = "found an exprChoice with outerPrec = " + outerPrec;
                        doTemp1 = Parser.tracer.print(tmp132, 742);
                        outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                        scrut3 = outerPrec$_ > prec;
                        if (scrut3 === true) {
                          tmp133 = Option.getOrElse(innerPrec, outerPrec$_);
                          scrut4 = parseKind(kind1, tmp133);
                          if (scrut4 instanceof Tree.Empty.class) {
                            tmp134 = Parser.tracer.print("nothing was parsed", 747);
                            return acc
                          } else {
                            rhs = scrut4;
                            tmp135 = Tree.summary(rhs);
                            tmp136 = "parsed " + tmp135;
                            tmp137 = Parser.tracer.print(tmp136, 750);
                            tmp138 = runtime.safeCall(process(rhs, runtime.Unit));
                            tmp139 = runtime.safeCall(tmp138(acc));
                            return termCont(tmp139, prec)
                          }
                        } else {
                          tmp140 = "the outer precedence is less than " + prec;
                          doTemp2 = Parser.tracer.print(tmp140, 752);
                          return acc
                        }
                      } else {
                        throw new globalThis.Error("match error");
                      }
                    } else if (scrut2 instanceof Option.None.class) {
                      tmp141 = "cannot consume " + token1;
                      tmp142 = Parser.tracer.print(tmp141, 755);
                      return acc
                    } else {
                      throw new globalThis.Error("match error");
                    }
                  }
                } else {
                  token1 = param01;
                  scrut2 = Parser.termInfixRule.exprChoice;
                  if (scrut2 instanceof Option.Some.class) {
                    param02 = scrut2.value;
                    if (globalThis.Array.isArray(param02) && param02.length === 5) {
                      first0 = param02[0];
                      first1 = param02[1];
                      first2 = param02[2];
                      first3 = param02[3];
                      first4 = param02[4];
                      kind1 = first0;
                      process = first1;
                      outerPrec = first2;
                      innerPrec = first3;
                      rest = first4;
                      tmp143 = "found an exprChoice with outerPrec = " + outerPrec;
                      doTemp1 = Parser.tracer.print(tmp143, 742);
                      outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                      scrut3 = outerPrec$_ > prec;
                      if (scrut3 === true) {
                        tmp144 = Option.getOrElse(innerPrec, outerPrec$_);
                        scrut4 = parseKind(kind1, tmp144);
                        if (scrut4 instanceof Tree.Empty.class) {
                          tmp145 = Parser.tracer.print("nothing was parsed", 747);
                          return acc
                        } else {
                          rhs = scrut4;
                          tmp146 = Tree.summary(rhs);
                          tmp147 = "parsed " + tmp146;
                          tmp148 = Parser.tracer.print(tmp147, 750);
                          tmp149 = runtime.safeCall(process(rhs, runtime.Unit));
                          tmp150 = runtime.safeCall(tmp149(acc));
                          return termCont(tmp150, prec)
                        }
                      } else {
                        tmp151 = "the outer precedence is less than " + prec;
                        doTemp2 = Parser.tracer.print(tmp151, 752);
                        return acc
                      }
                    } else {
                      throw new globalThis.Error("match error");
                    }
                  } else if (scrut2 instanceof Option.None.class) {
                    tmp152 = "cannot consume " + token1;
                    tmp153 = Parser.tracer.print(tmp152, 755);
                    return acc
                  } else {
                    throw new globalThis.Error("match error");
                  }
                }
              }
            } else {
              tmp154 = "keyword `" + name2;
              tmp155 = tmp154 + "` does not have infix rules";
              doTemp6 = Parser.tracer.print(tmp155, 728);
              name1 = param03;
              if (param12 === true) {
                scrut5 = runtime.safeCall(Precedence.Keywords.all.get(name1));
                if (scrut5 instanceof Option.None.class) {
                  tmp156 = "found an operator \"" + name1;
                  tmp157 = tmp156 + "\"";
                  doTemp3 = Parser.tracer.print(tmp157, 730);
                  scrut6 = Precedence.opPrec(name1);
                  if (globalThis.Array.isArray(scrut6) && scrut6.length === 2) {
                    first01 = scrut6[0];
                    first11 = scrut6[1];
                    leftPrec = first01;
                    rightPrec = first11;
                    tmp158 = "leftPrec = " + leftPrec;
                    tmp159 = tmp158 + "; rightPrec = ";
                    tmp160 = tmp159 + rightPrec;
                    doTemp4 = Parser.tracer.print(tmp160, 732);
                    scrut7 = leftPrec > prec;
                    if (scrut7 === true) {
                      tmp161 = consume();
                      tmp162 = Tree.Ident(name1, true);
                      op = tmp162;
                      tmp163 = term(rightPrec);
                      rhs1 = tmp163;
                      tmp164 = Stack.Cons(rhs1, Stack.Nil);
                      tmp165 = Stack.Cons(acc, tmp164);
                      tmp166 = Tree.App(op, tmp165);
                      return termCont(tmp166, prec)
                    } else {
                      return acc
                    }
                  } else {
                    token1 = param01;
                    scrut2 = Parser.termInfixRule.exprChoice;
                    if (scrut2 instanceof Option.Some.class) {
                      param02 = scrut2.value;
                      if (globalThis.Array.isArray(param02) && param02.length === 5) {
                        first0 = param02[0];
                        first1 = param02[1];
                        first2 = param02[2];
                        first3 = param02[3];
                        first4 = param02[4];
                        kind1 = first0;
                        process = first1;
                        outerPrec = first2;
                        innerPrec = first3;
                        rest = first4;
                        tmp167 = "found an exprChoice with outerPrec = " + outerPrec;
                        doTemp1 = Parser.tracer.print(tmp167, 742);
                        outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                        scrut3 = outerPrec$_ > prec;
                        if (scrut3 === true) {
                          tmp168 = Option.getOrElse(innerPrec, outerPrec$_);
                          scrut4 = parseKind(kind1, tmp168);
                          if (scrut4 instanceof Tree.Empty.class) {
                            tmp169 = Parser.tracer.print("nothing was parsed", 747);
                            return acc
                          } else {
                            rhs = scrut4;
                            tmp170 = Tree.summary(rhs);
                            tmp171 = "parsed " + tmp170;
                            tmp172 = Parser.tracer.print(tmp171, 750);
                            tmp173 = runtime.safeCall(process(rhs, runtime.Unit));
                            tmp174 = runtime.safeCall(tmp173(acc));
                            return termCont(tmp174, prec)
                          }
                        } else {
                          tmp175 = "the outer precedence is less than " + prec;
                          doTemp2 = Parser.tracer.print(tmp175, 752);
                          return acc
                        }
                      } else {
                        throw new globalThis.Error("match error");
                      }
                    } else if (scrut2 instanceof Option.None.class) {
                      tmp176 = "cannot consume " + token1;
                      tmp177 = Parser.tracer.print(tmp176, 755);
                      return acc
                    } else {
                      throw new globalThis.Error("match error");
                    }
                  }
                } else {
                  token1 = param01;
                  scrut2 = Parser.termInfixRule.exprChoice;
                  if (scrut2 instanceof Option.Some.class) {
                    param02 = scrut2.value;
                    if (globalThis.Array.isArray(param02) && param02.length === 5) {
                      first0 = param02[0];
                      first1 = param02[1];
                      first2 = param02[2];
                      first3 = param02[3];
                      first4 = param02[4];
                      kind1 = first0;
                      process = first1;
                      outerPrec = first2;
                      innerPrec = first3;
                      rest = first4;
                      tmp178 = "found an exprChoice with outerPrec = " + outerPrec;
                      doTemp1 = Parser.tracer.print(tmp178, 742);
                      outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                      scrut3 = outerPrec$_ > prec;
                      if (scrut3 === true) {
                        tmp179 = Option.getOrElse(innerPrec, outerPrec$_);
                        scrut4 = parseKind(kind1, tmp179);
                        if (scrut4 instanceof Tree.Empty.class) {
                          tmp180 = Parser.tracer.print("nothing was parsed", 747);
                          return acc
                        } else {
                          rhs = scrut4;
                          tmp181 = Tree.summary(rhs);
                          tmp182 = "parsed " + tmp181;
                          tmp183 = Parser.tracer.print(tmp182, 750);
                          tmp184 = runtime.safeCall(process(rhs, runtime.Unit));
                          tmp185 = runtime.safeCall(tmp184(acc));
                          return termCont(tmp185, prec)
                        }
                      } else {
                        tmp186 = "the outer precedence is less than " + prec;
                        doTemp2 = Parser.tracer.print(tmp186, 752);
                        return acc
                      }
                    } else {
                      throw new globalThis.Error("match error");
                    }
                  } else if (scrut2 instanceof Option.None.class) {
                    tmp187 = "cannot consume " + token1;
                    tmp188 = Parser.tracer.print(tmp187, 755);
                    return acc
                  } else {
                    throw new globalThis.Error("match error");
                  }
                }
              } else {
                token1 = param01;
                scrut2 = Parser.termInfixRule.exprChoice;
                if (scrut2 instanceof Option.Some.class) {
                  param02 = scrut2.value;
                  if (globalThis.Array.isArray(param02) && param02.length === 5) {
                    first0 = param02[0];
                    first1 = param02[1];
                    first2 = param02[2];
                    first3 = param02[3];
                    first4 = param02[4];
                    kind1 = first0;
                    process = first1;
                    outerPrec = first2;
                    innerPrec = first3;
                    rest = first4;
                    tmp189 = "found an exprChoice with outerPrec = " + outerPrec;
                    doTemp1 = Parser.tracer.print(tmp189, 742);
                    outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                    scrut3 = outerPrec$_ > prec;
                    if (scrut3 === true) {
                      tmp190 = Option.getOrElse(innerPrec, outerPrec$_);
                      scrut4 = parseKind(kind1, tmp190);
                      if (scrut4 instanceof Tree.Empty.class) {
                        tmp191 = Parser.tracer.print("nothing was parsed", 747);
                        return acc
                      } else {
                        rhs = scrut4;
                        tmp192 = Tree.summary(rhs);
                        tmp193 = "parsed " + tmp192;
                        tmp194 = Parser.tracer.print(tmp193, 750);
                        tmp195 = runtime.safeCall(process(rhs, runtime.Unit));
                        tmp196 = runtime.safeCall(tmp195(acc));
                        return termCont(tmp196, prec)
                      }
                    } else {
                      tmp197 = "the outer precedence is less than " + prec;
                      doTemp2 = Parser.tracer.print(tmp197, 752);
                      return acc
                    }
                  } else {
                    throw new globalThis.Error("match error");
                  }
                } else if (scrut2 instanceof Option.None.class) {
                  tmp198 = "cannot consume " + token1;
                  tmp199 = Parser.tracer.print(tmp198, 755);
                  return acc
                } else {
                  throw new globalThis.Error("match error");
                }
              }
            }
          } else {
            name1 = param03;
            if (param12 === true) {
              scrut5 = runtime.safeCall(Precedence.Keywords.all.get(name1));
              if (scrut5 instanceof Option.None.class) {
                tmp200 = "found an operator \"" + name1;
                tmp201 = tmp200 + "\"";
                doTemp3 = Parser.tracer.print(tmp201, 730);
                scrut6 = Precedence.opPrec(name1);
                if (globalThis.Array.isArray(scrut6) && scrut6.length === 2) {
                  first01 = scrut6[0];
                  first11 = scrut6[1];
                  leftPrec = first01;
                  rightPrec = first11;
                  tmp202 = "leftPrec = " + leftPrec;
                  tmp203 = tmp202 + "; rightPrec = ";
                  tmp204 = tmp203 + rightPrec;
                  doTemp4 = Parser.tracer.print(tmp204, 732);
                  scrut7 = leftPrec > prec;
                  if (scrut7 === true) {
                    tmp205 = consume();
                    tmp206 = Tree.Ident(name1, true);
                    op = tmp206;
                    tmp207 = term(rightPrec);
                    rhs1 = tmp207;
                    tmp208 = Stack.Cons(rhs1, Stack.Nil);
                    tmp209 = Stack.Cons(acc, tmp208);
                    tmp210 = Tree.App(op, tmp209);
                    return termCont(tmp210, prec)
                  } else {
                    return acc
                  }
                } else {
                  token1 = param01;
                  scrut2 = Parser.termInfixRule.exprChoice;
                  if (scrut2 instanceof Option.Some.class) {
                    param02 = scrut2.value;
                    if (globalThis.Array.isArray(param02) && param02.length === 5) {
                      first0 = param02[0];
                      first1 = param02[1];
                      first2 = param02[2];
                      first3 = param02[3];
                      first4 = param02[4];
                      kind1 = first0;
                      process = first1;
                      outerPrec = first2;
                      innerPrec = first3;
                      rest = first4;
                      tmp211 = "found an exprChoice with outerPrec = " + outerPrec;
                      doTemp1 = Parser.tracer.print(tmp211, 742);
                      outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                      scrut3 = outerPrec$_ > prec;
                      if (scrut3 === true) {
                        tmp212 = Option.getOrElse(innerPrec, outerPrec$_);
                        scrut4 = parseKind(kind1, tmp212);
                        if (scrut4 instanceof Tree.Empty.class) {
                          tmp213 = Parser.tracer.print("nothing was parsed", 747);
                          return acc
                        } else {
                          rhs = scrut4;
                          tmp214 = Tree.summary(rhs);
                          tmp215 = "parsed " + tmp214;
                          tmp216 = Parser.tracer.print(tmp215, 750);
                          tmp217 = runtime.safeCall(process(rhs, runtime.Unit));
                          tmp218 = runtime.safeCall(tmp217(acc));
                          return termCont(tmp218, prec)
                        }
                      } else {
                        tmp219 = "the outer precedence is less than " + prec;
                        doTemp2 = Parser.tracer.print(tmp219, 752);
                        return acc
                      }
                    } else {
                      throw new globalThis.Error("match error");
                    }
                  } else if (scrut2 instanceof Option.None.class) {
                    tmp220 = "cannot consume " + token1;
                    tmp221 = Parser.tracer.print(tmp220, 755);
                    return acc
                  } else {
                    throw new globalThis.Error("match error");
                  }
                }
              } else {
                token1 = param01;
                scrut2 = Parser.termInfixRule.exprChoice;
                if (scrut2 instanceof Option.Some.class) {
                  param02 = scrut2.value;
                  if (globalThis.Array.isArray(param02) && param02.length === 5) {
                    first0 = param02[0];
                    first1 = param02[1];
                    first2 = param02[2];
                    first3 = param02[3];
                    first4 = param02[4];
                    kind1 = first0;
                    process = first1;
                    outerPrec = first2;
                    innerPrec = first3;
                    rest = first4;
                    tmp222 = "found an exprChoice with outerPrec = " + outerPrec;
                    doTemp1 = Parser.tracer.print(tmp222, 742);
                    outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                    scrut3 = outerPrec$_ > prec;
                    if (scrut3 === true) {
                      tmp223 = Option.getOrElse(innerPrec, outerPrec$_);
                      scrut4 = parseKind(kind1, tmp223);
                      if (scrut4 instanceof Tree.Empty.class) {
                        tmp224 = Parser.tracer.print("nothing was parsed", 747);
                        return acc
                      } else {
                        rhs = scrut4;
                        tmp225 = Tree.summary(rhs);
                        tmp226 = "parsed " + tmp225;
                        tmp227 = Parser.tracer.print(tmp226, 750);
                        tmp228 = runtime.safeCall(process(rhs, runtime.Unit));
                        tmp229 = runtime.safeCall(tmp228(acc));
                        return termCont(tmp229, prec)
                      }
                    } else {
                      tmp230 = "the outer precedence is less than " + prec;
                      doTemp2 = Parser.tracer.print(tmp230, 752);
                      return acc
                    }
                  } else {
                    throw new globalThis.Error("match error");
                  }
                } else if (scrut2 instanceof Option.None.class) {
                  tmp231 = "cannot consume " + token1;
                  tmp232 = Parser.tracer.print(tmp231, 755);
                  return acc
                } else {
                  throw new globalThis.Error("match error");
                }
              }
            } else {
              token1 = param01;
              scrut2 = Parser.termInfixRule.exprChoice;
              if (scrut2 instanceof Option.Some.class) {
                param02 = scrut2.value;
                if (globalThis.Array.isArray(param02) && param02.length === 5) {
                  first0 = param02[0];
                  first1 = param02[1];
                  first2 = param02[2];
                  first3 = param02[3];
                  first4 = param02[4];
                  kind1 = first0;
                  process = first1;
                  outerPrec = first2;
                  innerPrec = first3;
                  rest = first4;
                  tmp233 = "found an exprChoice with outerPrec = " + outerPrec;
                  doTemp1 = Parser.tracer.print(tmp233, 742);
                  outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
                  scrut3 = outerPrec$_ > prec;
                  if (scrut3 === true) {
                    tmp234 = Option.getOrElse(innerPrec, outerPrec$_);
                    scrut4 = parseKind(kind1, tmp234);
                    if (scrut4 instanceof Tree.Empty.class) {
                      tmp235 = Parser.tracer.print("nothing was parsed", 747);
                      return acc
                    } else {
                      rhs = scrut4;
                      tmp236 = Tree.summary(rhs);
                      tmp237 = "parsed " + tmp236;
                      tmp238 = Parser.tracer.print(tmp237, 750);
                      tmp239 = runtime.safeCall(process(rhs, runtime.Unit));
                      tmp240 = runtime.safeCall(tmp239(acc));
                      return termCont(tmp240, prec)
                    }
                  } else {
                    tmp241 = "the outer precedence is less than " + prec;
                    doTemp2 = Parser.tracer.print(tmp241, 752);
                    return acc
                  }
                } else {
                  throw new globalThis.Error("match error");
                }
              } else if (scrut2 instanceof Option.None.class) {
                tmp242 = "cannot consume " + token1;
                tmp243 = Parser.tracer.print(tmp242, 755);
                return acc
              } else {
                throw new globalThis.Error("match error");
              }
            }
          }
        } else {
          token1 = param01;
          scrut2 = Parser.termInfixRule.exprChoice;
          if (scrut2 instanceof Option.Some.class) {
            param02 = scrut2.value;
            if (globalThis.Array.isArray(param02) && param02.length === 5) {
              first0 = param02[0];
              first1 = param02[1];
              first2 = param02[2];
              first3 = param02[3];
              first4 = param02[4];
              kind1 = first0;
              process = first1;
              outerPrec = first2;
              innerPrec = first3;
              rest = first4;
              tmp244 = "found an exprChoice with outerPrec = " + outerPrec;
              doTemp1 = Parser.tracer.print(tmp244, 742);
              outerPrec$_ = Option.getOrElse(outerPrec, Precedence.Keywords.maxOperatorPrec);
              scrut3 = outerPrec$_ > prec;
              if (scrut3 === true) {
                tmp245 = Option.getOrElse(innerPrec, outerPrec$_);
                scrut4 = parseKind(kind1, tmp245);
                if (scrut4 instanceof Tree.Empty.class) {
                  tmp246 = Parser.tracer.print("nothing was parsed", 747);
                  return acc
                } else {
                  rhs = scrut4;
                  tmp247 = Tree.summary(rhs);
                  tmp248 = "parsed " + tmp247;
                  tmp249 = Parser.tracer.print(tmp248, 750);
                  tmp250 = runtime.safeCall(process(rhs, runtime.Unit));
                  tmp251 = runtime.safeCall(tmp250(acc));
                  return termCont(tmp251, prec)
                }
              } else {
                tmp252 = "the outer precedence is less than " + prec;
                doTemp2 = Parser.tracer.print(tmp252, 752);
                return acc
              }
            } else {
              throw new globalThis.Error("match error");
            }
          } else if (scrut2 instanceof Option.None.class) {
            tmp253 = "cannot consume " + token1;
            tmp254 = Parser.tracer.print(tmp253, 755);
            return acc
          } else {
            throw new globalThis.Error("match error");
          }
        }
      } else if (scrut1 instanceof Stack.Nil.class) {
        return acc
      } else {
        throw new globalThis.Error("match error");
      }
    };
    typeExpr = function typeExpr(prec) {
      let tmp4, tmp5, tmp6, tmp7, tmp8;
      tmp4 = "typeExpr <<< " + prec;
      tmp5 = tmp4 + " ";
      tmp6 = TokenHelpers.preview(current);
      tmp7 = tmp5 + tmp6;
      tmp8 = () => {
        let scrut1, param01, param11, token1, param02, param12, name1, symbolic, scrut2, param03, keyword1, scrut3, param04, rule, scrut4, acc, param05, name2, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26;
        tmp9 = yeetSpaces();
        scrut1 = tmp9;
        if (scrut1 instanceof Stack.Cons.class) {
          param01 = scrut1.head;
          param11 = scrut1.tail;
          if (param01 instanceof Token.Identifier.class) {
            param02 = param01.name;
            param12 = param01.symbolic;
            if (param02 === "|") {
              tmp10 = consume();
              return typeExpr(prec)
            } else {
              name1 = param02;
              symbolic = param12;
              scrut2 = runtime.safeCall(Precedence.TypeKeywords.all.get(name1));
              if (scrut2 instanceof Option.Some.class) {
                param03 = scrut2.value;
                keyword1 = param03;
                scrut3 = runtime.safeCall(Parser.typeRule.keywordChoices.get(name1));
                if (scrut3 instanceof Option.Some.class) {
                  param04 = scrut3.value;
                  rule = param04;
                  scrut4 = keyword1.leftPrecOrMin > prec;
                  if (scrut4 === true) {
                    tmp11 = consume();
                    tmp12 = parseRule(keyword1.rightPrecOrMax, rule, Precedence.TypeKeywords.all);
                    acc = tmp12;
                    return typeExprCont(acc, prec)
                  } else {
                    tmp13 = "the left precedence is less" + name1;
                    tmp14 = Parser.tracer.print(tmp13, 780);
                    return Tree.empty
                  }
                } else if (scrut3 instanceof Option.None.class) {
                  tmp15 = "no rule starting with " + name1;
                  tmp16 = Parser.tracer.print(tmp15, 783);
                  return Tree.empty
                } else {
                  token1 = param01;
                  tmp17 = "unrecognized token: " + token1;
                  return Tree.error(tmp17)
                }
              } else if (scrut2 instanceof Option.None.class) {
                if (symbolic === true) {
                  tmp18 = consume();
                  tmp19 = "unexpected symbolic identifier: " + name1;
                  return Tree.error(tmp19)
                } else {
                  tmp20 = consume();
                  tmp21 = Tree.Ident(name1);
                  return typeExprCont(tmp21, prec)
                }
              } else {
                token1 = param01;
                tmp22 = "unrecognized token: " + token1;
                return Tree.error(tmp22)
              }
            }
          } else if (param01 instanceof Token.TypeVariable.class) {
            param05 = param01.name;
            name2 = param05;
            tmp23 = consume();
            tmp24 = "'" + name2;
            tmp25 = Tree.Ident(tmp24);
            return typeExprCont(tmp25, prec)
          } else {
            token1 = param01;
            tmp26 = "unrecognized token: " + token1;
            return Tree.error(tmp26)
          }
        } else {
          return Tree.error("unexpected end of input")
        }
      };
      return runtime.safeCall(Parser.tracer.trace(tmp7, (result) => {
        let tmp9;
        tmp9 = Tree.summary(result);
        return "typeExpr >>> " + tmp9
      }, tmp8))
    };
    typeExprCont = function typeExprCont(acc, prec) {
      let scrut1, doTemp, doTemp1, param01, param11, token1, param02, param12, name1, scrut2, scrut3, rhs, name2, scrut4, param03, keyword1, scrut5, param04, rule, doTemp2, doTemp3, scrut6, scrut7, param05, first4, first3, first2, first1, first0, kind1, process, outerPrec, innerPrec, rest, rhs1, acc$_, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26, tmp27, tmp28, tmp29, tmp30, tmp31, tmp32, tmp33, tmp34, tmp35, tmp36, tmp37, tmp38, tmp39, tmp40, tmp41, tmp42, tmp43, tmp44, tmp45, tmp46, tmp47, tmp48, tmp49, tmp50, tmp51, tmp52, tmp53, tmp54, tmp55, tmp56, tmp57, tmp58, tmp59, tmp60;
      tmp4 = ">>> typeExprCont " + prec;
      tmp5 = tmp4 + " ";
      tmp6 = Tree.summary(acc);
      tmp7 = tmp5 + tmp6;
      tmp8 = tmp7 + " <<<";
      tmp9 = Parser.tracer.print(tmp8, 796);
      tmp10 = yeetSpaces();
      scrut1 = tmp10;
      tmp11 = TokenHelpers.preview(current);
      tmp12 = "check keyword " + tmp11;
      doTemp = Parser.tracer.print(tmp12, 798);
      if (scrut1 instanceof Stack.Cons.class) {
        param01 = scrut1.head;
        param11 = scrut1.tail;
        if (param01 instanceof Token.Identifier.class) {
          param02 = param01.name;
          param12 = param01.symbolic;
          name2 = param02;
          scrut4 = runtime.safeCall(Precedence.TypeKeywords.all.get(name2));
          if (scrut4 instanceof Option.Some.class) {
            param03 = scrut4.value;
            keyword1 = param03;
            scrut5 = runtime.safeCall(Parser.typeInfixRule.keywordChoices.get(name2));
            if (scrut5 instanceof Option.Some.class) {
              param04 = scrut5.value;
              rule = param04;
              tmp13 = "the keyword is found in infix rules" + name2;
              doTemp2 = Parser.tracer.print(tmp13, 801);
              scrut6 = keyword1.leftPrecOrMin > prec;
              if (scrut6 === true) {
                scrut7 = rule.exprChoice;
                if (scrut7 instanceof Option.Some.class) {
                  param05 = scrut7.value;
                  if (globalThis.Array.isArray(param05) && param05.length === 5) {
                    first0 = param05[0];
                    first1 = param05[1];
                    first2 = param05[2];
                    first3 = param05[3];
                    first4 = param05[4];
                    kind1 = first0;
                    process = first1;
                    outerPrec = first2;
                    innerPrec = first3;
                    rest = first4;
                    tmp14 = consume();
                    tmp15 = parseKind(kind1, keyword1.rightPrecOrMin);
                    rhs1 = tmp15;
                    tmp16 = runtime.safeCall(process(rhs1));
                    tmp17 = runtime.safeCall(tmp16(acc));
                    acc$_ = tmp17;
                    return typeExprCont(acc$_, prec)
                  } else {
                    tmp18 = "keyword `" + name2;
                    tmp19 = tmp18 + "` does not have infix rules";
                    doTemp3 = Parser.tracer.print(tmp19, 810);
                    doTemp1 = Parser.tracer.print("not a keyword", 821);
                    name1 = param02;
                    if (param12 === false) {
                      scrut2 = Precedence.TypeKeywords.appPrec > prec;
                      if (scrut2 === true) {
                        tmp20 = Parser.tracer.print("found an application", 824);
                        scrut3 = typeExpr(Precedence.TypeKeywords.appPrec);
                        if (scrut3 instanceof Tree.Empty.class) {
                          return acc
                        } else if (scrut3 instanceof Tree.Error.class) {
                          return acc
                        } else {
                          rhs = scrut3;
                          tmp21 = Stack.Cons(acc, Stack.Nil);
                          tmp22 = Tree.App(rhs, tmp21);
                          return typeExprCont(tmp22, prec)
                        }
                      } else {
                        token1 = param01;
                        tmp23 = "cannot consume " + token1;
                        tmp24 = Parser.tracer.print(tmp23, 830);
                        return acc
                      }
                    } else {
                      token1 = param01;
                      tmp25 = "cannot consume " + token1;
                      tmp26 = Parser.tracer.print(tmp25, 830);
                      return acc
                    }
                  }
                } else if (scrut7 instanceof Option.None.class) {
                  return acc
                } else {
                  tmp27 = "keyword `" + name2;
                  tmp28 = tmp27 + "` does not have infix rules";
                  doTemp3 = Parser.tracer.print(tmp28, 810);
                  doTemp1 = Parser.tracer.print("not a keyword", 821);
                  name1 = param02;
                  if (param12 === false) {
                    scrut2 = Precedence.TypeKeywords.appPrec > prec;
                    if (scrut2 === true) {
                      tmp29 = Parser.tracer.print("found an application", 824);
                      scrut3 = typeExpr(Precedence.TypeKeywords.appPrec);
                      if (scrut3 instanceof Tree.Empty.class) {
                        return acc
                      } else if (scrut3 instanceof Tree.Error.class) {
                        return acc
                      } else {
                        rhs = scrut3;
                        tmp30 = Stack.Cons(acc, Stack.Nil);
                        tmp31 = Tree.App(rhs, tmp30);
                        return typeExprCont(tmp31, prec)
                      }
                    } else {
                      token1 = param01;
                      tmp32 = "cannot consume " + token1;
                      tmp33 = Parser.tracer.print(tmp32, 830);
                      return acc
                    }
                  } else {
                    token1 = param01;
                    tmp34 = "cannot consume " + token1;
                    tmp35 = Parser.tracer.print(tmp34, 830);
                    return acc
                  }
                }
              } else {
                tmp36 = "keyword `" + name2;
                tmp37 = tmp36 + "` does not have infix rules";
                doTemp3 = Parser.tracer.print(tmp37, 810);
                doTemp1 = Parser.tracer.print("not a keyword", 821);
                name1 = param02;
                if (param12 === false) {
                  scrut2 = Precedence.TypeKeywords.appPrec > prec;
                  if (scrut2 === true) {
                    tmp38 = Parser.tracer.print("found an application", 824);
                    scrut3 = typeExpr(Precedence.TypeKeywords.appPrec);
                    if (scrut3 instanceof Tree.Empty.class) {
                      return acc
                    } else if (scrut3 instanceof Tree.Error.class) {
                      return acc
                    } else {
                      rhs = scrut3;
                      tmp39 = Stack.Cons(acc, Stack.Nil);
                      tmp40 = Tree.App(rhs, tmp39);
                      return typeExprCont(tmp40, prec)
                    }
                  } else {
                    token1 = param01;
                    tmp41 = "cannot consume " + token1;
                    tmp42 = Parser.tracer.print(tmp41, 830);
                    return acc
                  }
                } else {
                  token1 = param01;
                  tmp43 = "cannot consume " + token1;
                  tmp44 = Parser.tracer.print(tmp43, 830);
                  return acc
                }
              }
            } else {
              doTemp1 = Parser.tracer.print("not a keyword", 821);
              name1 = param02;
              if (param12 === false) {
                scrut2 = Precedence.TypeKeywords.appPrec > prec;
                if (scrut2 === true) {
                  tmp45 = Parser.tracer.print("found an application", 824);
                  scrut3 = typeExpr(Precedence.TypeKeywords.appPrec);
                  if (scrut3 instanceof Tree.Empty.class) {
                    return acc
                  } else if (scrut3 instanceof Tree.Error.class) {
                    return acc
                  } else {
                    rhs = scrut3;
                    tmp46 = Stack.Cons(acc, Stack.Nil);
                    tmp47 = Tree.App(rhs, tmp46);
                    return typeExprCont(tmp47, prec)
                  }
                } else {
                  token1 = param01;
                  tmp48 = "cannot consume " + token1;
                  tmp49 = Parser.tracer.print(tmp48, 830);
                  return acc
                }
              } else {
                token1 = param01;
                tmp50 = "cannot consume " + token1;
                tmp51 = Parser.tracer.print(tmp50, 830);
                return acc
              }
            }
          } else {
            doTemp1 = Parser.tracer.print("not a keyword", 821);
            name1 = param02;
            if (param12 === false) {
              scrut2 = Precedence.TypeKeywords.appPrec > prec;
              if (scrut2 === true) {
                tmp52 = Parser.tracer.print("found an application", 824);
                scrut3 = typeExpr(Precedence.TypeKeywords.appPrec);
                if (scrut3 instanceof Tree.Empty.class) {
                  return acc
                } else if (scrut3 instanceof Tree.Error.class) {
                  return acc
                } else {
                  rhs = scrut3;
                  tmp53 = Stack.Cons(acc, Stack.Nil);
                  tmp54 = Tree.App(rhs, tmp53);
                  return typeExprCont(tmp54, prec)
                }
              } else {
                token1 = param01;
                tmp55 = "cannot consume " + token1;
                tmp56 = Parser.tracer.print(tmp55, 830);
                return acc
              }
            } else {
              token1 = param01;
              tmp57 = "cannot consume " + token1;
              tmp58 = Parser.tracer.print(tmp57, 830);
              return acc
            }
          }
        } else {
          doTemp1 = Parser.tracer.print("not a keyword", 821);
          token1 = param01;
          tmp59 = "cannot consume " + token1;
          tmp60 = Parser.tracer.print(tmp59, 830);
          return acc
        }
      } else {
        doTemp1 = Parser.tracer.print("not a keyword", 821);
        if (scrut1 instanceof Stack.Nil.class) {
          return acc
        } else {
          throw new globalThis.Error("match error");
        }
      }
    };
    mod = function mod(acc) {
      let scrut1, doTemp, param01, param11, param02, param12, name1, scrut2, param03, keyword1, scrut3, param04, rule, tree1, scrut4, param05, rule1, tree2, param06, param13, bindings, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, tmp20;
      tmp4 = yeetSpaces();
      scrut1 = tmp4;
      doTemp = Parser.tracer.print(">>>>>> mod <<<<<<", 835);
      if (scrut1 instanceof Stack.Cons.class) {
        param01 = scrut1.head;
        param11 = scrut1.tail;
        if (param01 instanceof Token.Identifier.class) {
          param02 = param01.name;
          param12 = param01.symbolic;
          if (param02 === ";;") {
            tmp5 = consume();
            return mod
          } else {
            name1 = param02;
            scrut2 = runtime.safeCall(Precedence.Keywords.all.get(name1));
            if (scrut2 instanceof Option.Some.class) {
              param03 = scrut2.value;
              keyword1 = param03;
              scrut4 = runtime.safeCall(Parser.termRule.keywordChoices.get(name1));
              if (scrut4 instanceof Option.Some.class) {
                param05 = scrut4.value;
                rule1 = param05;
                tmp6 = term(0);
                tree2 = tmp6;
                if (tree2 instanceof Tree.LetIn.class) {
                  param06 = tree2.bindings;
                  param13 = tree2.body;
                  bindings = param06;
                  if (param13 instanceof Tree.Empty.class) {
                    tmp7 = Tree.DefineKind.Let(false);
                    tmp8 = Tree.Define(tmp7, bindings);
                    tmp9 = Stack.Cons(tmp8, acc);
                    return modCont(tmp9)
                  } else {
                    tmp10 = Stack.Cons(tree2, acc);
                    return modCont(tmp10)
                  }
                } else {
                  tmp11 = Stack.Cons(tree2, acc);
                  return modCont(tmp11)
                }
              } else {
                scrut3 = runtime.safeCall(Parser.declRule.keywordChoices.get(name1));
                if (scrut3 instanceof Option.Some.class) {
                  param04 = scrut3.value;
                  rule = param04;
                  tmp12 = consume();
                  tmp13 = parseRule(0, rule, Precedence.Keywords.all);
                  tree1 = tmp13;
                  tmp14 = Stack.Cons(tree1, acc);
                  return modCont(tmp14)
                } else {
                  tmp15 = term(0, Option.None);
                  tmp16 = Stack.Cons(tmp15, acc);
                  return modCont(tmp16)
                }
              }
            } else {
              tmp17 = term(0, Option.None);
              tmp18 = Stack.Cons(tmp17, acc);
              return modCont(tmp18)
            }
          }
        } else {
          tmp19 = term(0, Option.None);
          tmp20 = Stack.Cons(tmp19, acc);
          return modCont(tmp20)
        }
      } else if (scrut1 instanceof Stack.Nil.class) {
        return Stack.reverse(acc)
      } else {
        throw new globalThis.Error("match error");
      }
    };
    modCont = function modCont(acc) {
      let scrut1, doTemp, param01, param11, param02, param12, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9;
      tmp4 = yeetSpaces();
      scrut1 = tmp4;
      doTemp = Parser.tracer.print(">>>>>> modCont <<<<<<", 856);
      if (scrut1 instanceof Stack.Cons.class) {
        param01 = scrut1.head;
        param11 = scrut1.tail;
        if (param01 instanceof Token.Identifier.class) {
          param02 = param01.name;
          param12 = param01.symbolic;
          if (param02 === ";;") {
            tmp5 = consume();
            return mod(acc)
          } else {
            tmp6 = parseRule(0, Parser.declRule, Precedence.Keywords.all);
            tmp7 = Stack.Cons(tmp6, acc);
            return modCont(tmp7)
          }
        } else {
          tmp8 = parseRule(0, Parser.declRule, Precedence.Keywords.all);
          tmp9 = Stack.Cons(tmp8, acc);
          return modCont(tmp9)
        }
      } else if (scrut1 instanceof Stack.Nil.class) {
        return Stack.reverse(acc)
      } else {
        throw new globalThis.Error("match error");
      }
    };
    current = tokens;
    counter = 0;
    tmp = runtime.safeCall(Parser.tracer.trace("module <<< ", (result) => {
      let tmp4;
      tmp4 = Tree.summary(result);
      return "module >>> " + tmp4
    }, () => {
      return mod(Stack.Nil)
    }));
    tree = tmp;
    tmp1 = yeetSpaces();
    scrut = tmp1;
    if (scrut instanceof Stack.Cons.class) {
      param0 = scrut.head;
      param1 = scrut.tail;
      token = param0;
      tmp2 = "expect EOF instead of " + token;
      message = tmp2;
      tmp3 = Parser.tracer.print(message, 871);
      return Tree.Error(tree, message)
    } else if (scrut instanceof Stack.Nil.class) {
      return tree
    } else {
      throw new globalThis.Error("match error");
    }
  }
  static toString() { return "Parser"; }
};
let Parser = Parser1; export default Parser;
