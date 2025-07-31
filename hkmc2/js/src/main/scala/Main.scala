package hkmc2

import scala.util.Try
import scala.scalajs.js.annotation.*
import org.scalajs.dom.{Event, TextEvent, UIEvent, HTMLTextAreaElement, document}
import mlscript.utils._
// import mlscript._
import hkmc2._
import mlscript.utils.shorthands._
import scala.util.matching.Regex
import scala.scalajs.js
import scala.collection.immutable
import semantics.{Elaborator, Resolver}
import semantics.Elaborator.{Ctx, State}
import invalml.{InvalTyper, InvalCtx}
import hkmc2.utils.TraceLogger
import syntax.Lexer
import collection.mutable.{Buffer, Map as MutMap}
import js.JSConverters.*
import codegen.js.{JSBuilder, JSBuilderArgNumSanityChecks}

@JSExportTopLevel("default")
object Main {
  enum Stage:
    case Lexer, Parser, Elaborator, Resolver, Typing, Lowering, Generation
  
  private def println(x: Any): Unit = Predef.println(s"[MLscript.compile] $x")
  
  // A few debug flags to control the output.
  private var debugParsing = false
  private var debugElaboration = false
  private var debugResolving = false
  private var debugTyping = false
  private var debugLowering = false
  private var debugCodeGeneration = false
  
  private val debug = false
  
  /** The default output function does not print anything in the browser. */
  def noOutput(msg: => Any): Unit = ()
  
  val etl = new utils.TraceLogger:
    override protected def emitDbg(str: Str): Unit = output(Stage.Elaborator, str)
    override def doTrace: Bool = debugElaboration
  val rtl = new utils.TraceLogger:
    override protected def emitDbg(str: Str): Unit = output(Stage.Resolver, str)
    override def doTrace: Bool = debugResolving
  val ltl = new utils.TraceLogger:
    override protected def emitDbg(str: Str): Unit = output(Stage.Lowering, str)
    override def doTrace: Bool = debugTyping
  val jtl = new utils.TraceLogger:
    override protected def emitDbg(str: Str): Unit = output(Stage.Generation, str)
    override def doTrace: Bool = debugCodeGeneration
  
  extension (ty: invalml.GeneralType)
    def display(using utils.Scope, invalml.InvalCtx): Str =
      val builder = new StringBuilder
      val printer = invalml.PrettyPrinter: line => 
        builder ++= line; builder += '\n'
      printer.print(ty)
      builder.toString
  
  private val traces = Buffer.empty[(Stage, Str)]
  
  private def output(stage: Stage, message: Str): Unit =
    traces += ((stage, message))
  
  private def getOutput(stage: Stage): Str =
    traces.iterator.filter(_._1 == stage).map(_._2).mkString("\n")
  
  /** A centralized place to collect all diagnostics. */
  private val diagnostics = Buffer.empty[(Stage, Diagnostic)]
  
  /** Make a new `Raise` function for a specific stage. */
  private def raise(stage: Stage)(d: Diagnostic): Unit = diagnostics += ((stage, d))
  
  /** Collect diagnostics raised in a specific stage. */
  private def filterDiagnostics(stage: Stage): js.Array[Diagnostic] =
    diagnostics.iterator.filter(_._1 == stage).map(_._2).toJSArray
  
  /** Generate HTML fragments from diagnostics raised in a specific stage. */
  private def getDiagnosticsHTML(stage: Stage): js.Array[Str] =
    diagnostics.iterator.filter(_._1 == stage).map(_._2 |> report).toJSArray
  
  /** Access the compiler using `MLscript.compile` in JavaScript. */
  @JSExport
  def compile(source: String, options: js.Dynamic): js.Dynamic = {
    println(s"Options: ${js.JSON.stringify(options)}")
    
    if js.typeOf(options) == "object" && options != null then
      debugParsing = options.debugParsing === true
      debugElaboration = options.debugElaboration === true
      debugResolving = options.debugResolving === true
      debugTyping = options.debugTyping === true
      debugLowering = options.debugLowering === true
      debugCodeGeneration = options.debugCodeGeneration === true
    
    traces.clear()
    diagnostics.clear()
    
    println(s"Input: $source")
    
    val origin = Origin("source.mls", 1, new FastParseHelpers(source))
    
    // From `MLsDiffMaker`
    
    given State = new State:
      override def dbg: Bool =
        debugParsing || debugElaboration || debugResolving || debug
    
    val baseScp: utils.Scope = utils.Scope.empty
    
    var curCtx = State.init
    
    given Config = Config.default // TODO: Support custom config?
    
    val lexer = new Lexer(origin, dbg = debugParsing)(using raise(Stage.Lexer))
    val tokens = lexer.bracketedTokens
    
    // TODO: Maybe these debug outputs can be printed in some places.
    // if showParse.isSet || dbgParsing.isSet then
    //   output(syntax.Lexer.printTokens(tokens))
    println(s"Tokens: ${Lexer.printTokens(tokens)}")
    
    val rules = syntax.ParseRules()
    val p = new syntax.Parser(origin, tokens, rules, raise(Stage.Parser), dbg = debugParsing):
      def doPrintDbg(msg: => Str): Unit = if dbg then output(Stage.Parser, msg)
    val res = p.parseAll(p.block(allowNewlines = true))
    
    // TODO: Maybe these debug outputs can be printed in some places.
    // // If parsed tree is displayed, don't show the string serialization.
    // if (parseOnly.isSet || showParse.isSet) && !showParsedTree.isSet then
    //   output(s"Parsed:${res.map("\n\t"+_.showDbg).mkString}")

    // showParsedTree.get.foreach: post =>
    //   output(s"Parsed tree:")
    //   res.foreach(t => output(t.showAsTree(using post)))
    
    res.foreach(t => println(t.showAsTree))
    
    // if showParse.isSet then
    //   output(s"AST: $res")
    
    // if showContext.isSet then
    //   output("Env:")
    //   curCtx.env.foreach: (k, v) =>
    //     if !(oldCtx.env contains k) then
    //       output(s"  $k -> $v")
    
    var curICtx = Resolver.ICtx.empty
    
    import invalml.*, utils.Scope
    given Scope = Scope.empty
    given btl: TraceLogger = new utils.TraceLogger:
      override protected def emitDbg(str: Str): Unit = output(Stage.Typing, str)
      override def doTrace: Bool = true
    given invalml.InvalCtx =
      given Ctx = curCtx
      invalml.InvalCtx.init(raise(Stage.Typing))
    val typer = InvalTyper()
    
    def importFile(filePath: Str, moduleName: Str, source: Str, verbose: Bool)(using Config): Unit =
      
      // val raise: Raise = throw _
      // given raise: Raise = d =>
      //   output(s"Error: $d")
      //   ()
      
      val fph = new FastParseHelpers(source)
      val origin = Origin(filePath, 0, fph)
      
      val lexer = new syntax.Lexer(origin, dbg = debugParsing)(using raise(Stage.Lexer))
      val tokens = lexer.bracketedTokens
      
      if debugParsing then
        println(syntax.Lexer.printTokens(tokens))
      
      val rules = syntax.ParseRules()
      val p = new syntax.Parser(origin, tokens, rules, raise(Stage.Parser), dbg = debugParsing):
        def doPrintDbg(msg: => Str): Unit = if dbg then noOutput(msg)
      val res = p.parseAll(p.block(allowNewlines = true))
      val imprtSymbol =
        semantics.TopLevelSymbol("import#" + moduleName)
      given Elaborator.Ctx = curCtx.nestLocal
      val elab = raise(Stage.Elaborator).givenIn:
        Elaborator(etl, new WebImporter)
      try
        val resBlk = new syntax.Tree.Block(res)
        val (e, newCtx) = elab.importFrom(resBlk)
        val ctxWithImports = newCtx.withMembers(resBlk.definedSymbols)
        if verbose then
          println(s"Imported ${resBlk.definedSymbols.size} member(s)")
        curCtx = ctxWithImports
        val resolver = semantics.Resolver(rtl)(using raise(Stage.Resolver), State)
        curICtx = resolver.traverseBlock(e)(using curICtx)
        typer.typePurely(e)
      catch
        case err: Throwable =>
          println(err)
    
    importFile("Prelude.mls", "Prelude", invalPreludeSource, verbose = true)
    importFile("Runtime.mls", "Runtime", WebImporter.fileNameSourceMap("Runtime.mls")._1, verbose = true)
    
    
    val elab = raise(Stage.Elaborator).givenIn:
      semantics.Elaborator(etl, new WebImporter)
    // val blockSymbol =
    //   semantics.TopLevelSymbol("block#"+blockNum)
    // blockNum += 1
    // given Ctx = curCtx.nest(S(blockSymbol))
    given Ctx = curCtx.nestLocal
    
    val blk = new syntax.Tree.Block(res)
    val (e, newCtx) = elab.topLevel(blk)
    
    
    curCtx = newCtx
    // If elaborated tree is displayed, don't show the string serialization.
    // if (showElab.isSet || debug.isSet) && !showElaboratedTree.isSet then
    //   output(s"Elab: ${e.showDbg}")
    // showElaboratedTree.get.foreach: post =>
    //   output(s"Elaborated tree:")
    //   output(e.showAsTree(using post))
    val elaboratedTree = e.showAsTree
    
    val resolver = semantics.Resolver(rtl)(using raise(Stage.Resolver), State)
    curICtx = resolver.traverseBlock(e)(using curICtx)
    // TODO: Put the debug output at somewhere.
    // if showResolve.isSet then
    //   output(s"Resolved: ${trm.showDbg}")
    // showResolvedTree.get.foreach: post =>
    //   output(s"Resolved tree:")
    //   output(trm.showAsTree(using post))
    println("Resolved tree:\n" + e.showAsTree)
    
    
    // From `BbmlDiffMaker`
    
    val ty = typer.typePurely(e)
    val simplif = TypeSimplifier(btl)
    val sty = simplif(true, 0)(ty)
    
    // From `JSBackendDiffMaker`
    
    val low = ltl.givenIn:
      given Raise = raise(Stage.Lowering)
      new codegen.Lowering()
        with codegen.LoweringSelSanityChecks
    val jsb = jtl.givenIn:
      new JSBuilder with JSBuilderArgNumSanityChecks
    
    val e2 = new semantics.Term.Blk(
      semantics.Import(State.runtimeSymbol, "Runtime.mjs") :: e.stats,
      e.res
    )
    
    val lowered0 = low.program(e2)
    val nestedScp = baseScp
    
    val (pre, code) = nestedScp.givenIn:
      given Raise = raise(Stage.Generation)
      jsb.worksheet(lowered0)
      
    js.Dynamic.literal(
      lexer = js.Dynamic.literal(
        tokens = lexer.tokens.iterator.map(_._1.describe).toJSArray,
        diagnostics = getDiagnosticsHTML(Stage.Lexer),
      ),
      parser = js.Dynamic.literal(
        trees = res.iterator.map(_.showAsTree).toJSArray,
        diagnostics = getDiagnosticsHTML(Stage.Parser),
        traces = getOutput(Stage.Parser)
      ),
      elaborator = js.Dynamic.literal(
        tree = elaboratedTree,
        traces = getOutput(Stage.Elaborator),
        diagnostics = getDiagnosticsHTML(Stage.Elaborator),
      ),
      resolver = js.Dynamic.literal(
        tree = e.showAsTree,
        traces = getOutput(Stage.Resolver),
        diagnostics = getDiagnosticsHTML(Stage.Resolver),
      ),
      typer = js.Dynamic.literal(
        untouchedType = ty.display,
        simplifiedType = sty.display,
        traces = getOutput(Stage.Typing),
        diagnostics = getDiagnosticsHTML(Stage.Typing),
      ),
      lowering = js.Dynamic.literal(
        tree = lowered0.showAsTree,
        traces = getOutput(Stage.Lowering),
        diagnostics = getDiagnosticsHTML(Stage.Lowering),
      ),
      codegen = js.Dynamic.literal(
        vars = pre.toString,
        code = code.toString,
        traces = getOutput(Stage.Generation),
        diagnostics = getDiagnosticsHTML(Stage.Generation),
      ),
    )
    
    // val tryRes = try {
    //   // import fastparse._
    //   // import fastparse.Parsed.{Success, Failure}
    //   // import mlscript.{NewParser, ErrorReport, Origin}
    //   // val lines = str.splitSane('\n').toIndexedSeq
    //   // val processedBlock = lines.mkString
    //   // val fph = new mlscript.FastParseHelpers(str, lines)
    //   // val origin = Origin("<input>", 1, fph)
    //   // val lexer = new NewLexer(origin, throw _, dbg = false)
    //   // val tokens = lexer.bracketedTokens
    //   // val parser = new NewParser(origin, tokens, newDefs = true, throw _, dbg = false, N) {
    //   //   def doPrintDbg(msg: => Str): Unit = if (dbg) println(msg)
    //   // }
    //   // parser.parseAll(parser.typingUnit) match {
    //   //   case tu =>
    //   //     val pgrm = Pgrm(tu.entities)
    //   //     println(s"Parsed: $pgrm")
          
    //   //     val typer = new mlscript.Typer(
    //   //       dbg = false,
    //   //       verbose = false,
    //   //       explainErrors = false,
    //   //       newDefs = true,
    //   //     )
          
    //   //     import typer._

    //   //     implicit val raise: Raise = throw _
    //   //     implicit var ctx: Ctx = Ctx.init
    //   //     implicit val extrCtx: Opt[typer.ExtrCtx] = N

    //   //     val vars: Map[Str, typer.SimpleType] = Map.empty
    //   //     val tpd = typer.typeTypingUnit(tu, N)(ctx.nest, raise, vars)
          
    //   //     object SimplifyPipeline extends typer.SimplifyPipeline {
    //   //       def debugOutput(msg: => Str): Unit =
    //   //         // if (mode.dbgSimplif) output(msg)
    //   //         println(msg)
    //   //     }
    //   //     val sim = SimplifyPipeline(tpd, S(true))(ctx)
          
    //   //     val exp = typer.expandType(sim)(ctx)
          
    //   //     val expStr = exp.showIn(0)(ShowCtx.mk(exp :: Nil, newDefs = true)).stripSuffix("\n")
    //   //       .replaceAll("  ", "&nbsp;&nbsp;")
    //   //       .replaceAll("\n", "<br/>")

    //   //     // TODO format HTML better
    //   //     val typingStr = """<div><table width="100%">
    //   //                       |  <tr>
    //   //                       |    <td colspan="2"><h4><i>Typing Results:</i></h4></td>
    //   //                       |  </tr>
    //   //                       |""".stripMargin +
    //   //                    s"""<tr>
    //   //                       |  ${s"<td colspan=\"2\">${expStr}</td>"}
    //   //                       |</tr>
    //   //                       |""".stripMargin

    //   //     val backend = new JSWebBackend()
    //   //     val (lines, resNames) = backend(pgrm)
    //   //     val code = lines.mkString("\n")

    //   //     // TODO: add a toggle button to show js code
    //   //     // val jsStr = ("\n\n=====================JavaScript Code=====================\n" + code)
    //   //     //   .stripSuffix("\n")
    //   //     //   .replaceAll("  ", "&nbsp;&nbsp;")
    //   //     //   .replaceAll("\n", "<br/>")

    //   //     val exe = executeCode(code) match {
    //   //       case Left(err) => err
    //   //       case Right(lines) => generateResultTable(resNames.zip(lines))
    //   //     }

    //   //     val resStr = ("""<tr>
    //   //                     |  <td colspan="2"><h4><i>Execution Results:</i></h4></td>
    //   //                     |</tr>
    //   //                     |""".stripMargin + exe + "</table>")
          
    //   //     typingStr + resStr
    //   // }
    //   ()
    // } catch {
    //   // case err: ErrorReport =>
    //   case err: Diagnostic =>
    //     report(err)
    //   case err: Throwable =>
    //     s"""
    //   <font color="Red">
    //   Unexpected error: ${err}${
    //       err.printStackTrace
    //       // err.getStackTrace().map(s"$htmlLineBreak$htmlWhiteSpace$htmlWhiteSpace at " + _).mkString
    //       ""
    //     }</font>"""
    // }
    
    // target.innerHTML = tryRes
  }
  
  def underline(fragment: Str): Str =
    s"<u style=\"text-decoration: #E74C3C dashed underline\">$fragment</u>"
  
  var totalTypeErrors = 0
  var totalWarnings = 0
  var outputMarker = ""
  val blockLineNum = 0
  val showRelativeLineNums = false
  
  def report(diag: Diagnostic): Str =
    var sb = new collection.mutable.StringBuilder
    def output(s: Str): Unit =
      sb ++= outputMarker
      sb ++= s
      sb ++= htmlLineBreak
      ()
    val sctx = Message.mkCtx(diag.allMsgs.iterator.map(_._1))
    val headStr = diag match
      case ErrorReport(mainMsg, allMsg, loco, src) =>
        totalTypeErrors += 1
        s"╔══ <strong style=\"color: #E74C3C\">[ERROR]</strong> "
      case WarningReport(mainMsg, allMsg, loco, src) =>
        totalWarnings += 1
        s"╔══ <strong style=\"color: #F39C12\">[WARNING]</strong> "
      case InternalError(mainMsg, allMsgs, src) => 
        s"╔══ <strong style=\"color: #8E44AD\">[INTERNAL ERROR]</strong> "
    val lastMsgNum = diag.allMsgs.size - 1
    var globalLineNum =
      blockLineNum // solely used for reporting useful test failure messages
    diag.allMsgs.zipWithIndex.foreach { case ((msg, loco), msgNum) =>
      val isLast = msgNum =:= lastMsgNum
      val msgStr = msg.showIn(sctx)
      if msgNum =:= 0 then
        output(headStr + msgStr)
      else
        output(s"${if isLast && loco.isEmpty then "╙──" else "╟──"} ${msgStr}")
      if loco.isEmpty && diag.allMsgs.size =:= 1 then output("╙──")
      loco.foreach: loc =>
        val (startLineNum, startLineStr, startLineCol) =
          loc.origin.fph.getLineColAt(loc.spanStart)
        if globalLineNum =:= 0 then globalLineNum += startLineNum - 1
        val (endLineNum, endLineStr, endLineCol) =
          loc.origin.fph.getLineColAt(loc.spanEnd)
        var l = startLineNum
        var c = startLineCol // c starts from 1
        while l <= endLineNum do
          val globalLineNum = loc.origin.startLineNum + l - 1
          val relativeLineNum = globalLineNum - blockLineNum + 1
          val shownLineNum =
            if showRelativeLineNums && relativeLineNum > 0 then
              s"l.+$relativeLineNum"
            else "l." + globalLineNum
          val prepre = "║  "
          val pre = s"$shownLineNum: " // Looks like l.\d+
          val curLine = loc.origin.fph.lines(l - 1)
          val lastCol =
            if l =:= endLineNum then endLineCol else curLine.length + 1
          val front = curLine.slice(0, c - 1)
          val middle = underline(curLine.slice(c - 1, lastCol - 1))
          val back = curLine.slice(lastCol - 1, curLine.length)
          output(s"$prepre$pre\t$front$middle$back")
          c = 1
          l += 1
          if isLast then output("╙──")
    }
    if diag.allMsgs.isEmpty then output("╙──")
    sb.toString

  // Execute the generated code.
  // We extract this function because there is some boilerplate code.
  // It returns a tuple of three items:
  // 1. results of definitions;
  // 2. results of expressions;
  // 3. error message (if has).
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  private def executeCode(code: Str): Either[Str, Ls[Str]] = {
    try {
      R(js.eval(code).asInstanceOf[js.Array[Str]].toList)
    } catch {
      case e: Throwable =>
        val errorBuilder = new StringBuilder()
        errorBuilder ++= "<font color=\"red\">Runtime error occurred:</font>"
        errorBuilder ++= htmlLineBreak + e.getMessage
        errorBuilder ++= htmlLineBreak
        errorBuilder ++= htmlLineBreak
        L(errorBuilder.toString)
    }
  }

  private def generateResultTable(res: Ls[(Str, Str)]): Str = {
    val htmlBuilder = new StringBuilder
    htmlBuilder ++= """<tr>
                      |  <td>Name</td>
                      |  <td>Value</td>
                      |</tr>
                      |""".stripMargin

    res.foreach(value => {
      htmlBuilder ++= s"""<tr>
                         |  <td class="name">${value._1.replaceAll("  ", "&nbsp;&nbsp;").replaceAll("\n", "<br/>")}</td>
                         |  ${s"<td>${value._2.replaceAll("  ", "&nbsp;&nbsp;").replaceAll("\n", "<br/>")}</td>"}
                         |</tr>
                         |""".stripMargin
    })

    htmlBuilder.toString
  }
  
  private val htmlLineBreak = "<br />"
  private val htmlWhiteSpace = "&nbsp;"
  
  private val invalPreludeSource = """
declare class Any
declare class Nothing
declare class Object

declare class untyped

declare class Unit
declare class Bool
declare class Int
declare class Num

data
  class
    CodeBase[T, C, S]
    Region[T]
    Ref[T, S](reg: Region[T], value: S)

@untyped
set
  globalThis.CodeBase = CodeBase
  globalThis.Region = Region
  globalThis.Ref = Ref

declare class Function

declare class Str(length: Int, concat: Str -> Str)

declare class Error(msg: Str)


declare module source with
  object
    line: Int
    name: Str
    file: Str

declare module js with
  fun try_catch

declare module debug with
  fun printStack
  fun getLocals

declare module annotations with
  object compile


declare fun run: [T] -> CodeBase[out T, out Nothing, out Any] -> T
declare fun log: Str -> Any
declare fun error: Nothing

declare fun (+): (Int, Int) -> Int
declare fun (-): (Int, Int) -> Int
declare fun (*): (Int, Int) -> Int
declare fun (/): (Int, Int) -> Num
declare fun (+.): (Num, Num) -> Num
declare fun (-.): (Num, Num) -> Num
declare fun (*.): (Num, Num) -> Num
declare fun (/.): (Num, Num) -> Num

declare fun (<): (Int, Int) -> Bool
declare fun (>): (Int, Int) -> Bool
declare fun (<=): (Int, Int) -> Bool
declare fun (>=): (Int, Int) -> Bool
declare fun (==): [T] -> (T, T) -> Bool
declare fun (!=): [T] -> (T, T) -> Bool

declare fun (&&): (Bool, Bool) -> Bool
declare fun (||): (Bool, Bool) -> Bool


fun print(x) = @untyped globalThis.console.log(x)
"""
}

