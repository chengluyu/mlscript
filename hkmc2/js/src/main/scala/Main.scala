package hkmc2

import scala.util.Try
import mlscript.utils.*, shorthands.*
import scala.util.matching.Regex
import scala.scalajs.js, js.annotation.*
import collection.immutable, collection.mutable.Buffer
import semantics.{Elaborator, Resolver}, Elaborator.{Ctx, State}
import utils.path.*
import scala.scalajs.js.JSConverters.*
import syntax.{Lexer, ParseRules}
import typing.*
import typing.supremef.*

@JSExportTopLevel("default")
object Main:
  enum Stage:
    case Lexer, Parser, Elaborator, Resolver, Typer
  
  private def println(x: Any): Unit = Predef.println(s"[MLscript.compile] $x")
  
  // A few debug flags to control the output.
  private var debugParsing = false
  private var debugElaboration = false
  private var debugResolving = false
  private var debugTyper = false
  
  private val debug = false
  
  /** The default output function does not print anything in the browser. */
  def noOutput(msg: => Any): Unit = ()
  
  val etl = new utils.TraceLogger:
    override protected def emitDbg(str: Str): Unit = output(Stage.Elaborator, str)
    override def doTrace: Bool = debugElaboration
  val rtl = new utils.TraceLogger:
    override protected def emitDbg(str: Str): Unit = output(Stage.Resolver, str)
    override def doTrace: Bool = debugResolving
  
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
  
  given State = new State:
    override def dbg: Bool =
      debugParsing || debugElaboration || debugResolving || debug
  
  private val importer = new semantics.importer.DummyImporter
  
  /** Access the compiler using `MLscript.compile` in JavaScript. */
  @JSExport
  def compile(blocks: js.Array[js.Tuple2[Int, Str]], options: js.Dynamic): js.Dynamic =
    traces.clear()
    diagnostics.clear()
    
    println(s"Options: ${js.JSON.stringify(options)}")
    if js.typeOf(options) == "object" && options != null then
      debugParsing = options.debugParsing === true
      debugElaboration = options.debugElaboration === true
      debugResolving = options.debugResolving === true
      debugTyper = options.debugTyper === true
    
    val allTokens = Buffer.empty[Str]
    val allParsedTrees = Buffer.empty[Str]
    val allElaboratedTrees = Buffer.empty[Str]
    val allResolvedTrees = Buffer.empty[Str]
    val allTypeCheckOutputs = Buffer.empty[js.Dynamic]
    
    val baseScope: utils.Scope = utils.Scope.empty
      var curCtx = State.init
    
    blocks.foreach: block =>
      val origin = Origin(AbsolutePath("source.mls"), block._1, new FastParseHelpers(block._2))
      given Config = Config.default // TODO: Support custom config?
      val lexer = new Lexer(origin, dbg = debugParsing)(using raise(Stage.Lexer))
      val tokens = lexer.bracketedTokens
      val rules = syntax.ParseRules()
      val p = new syntax.Parser(origin, tokens, rules, raise(Stage.Parser), dbg = debugParsing):
        def doPrintDbg(msg: => Str): Unit = if dbg then output(Stage.Parser, msg)
      val parsedTrees = p.parseAll(p.block(allowNewlines = true))
      var curICtx = Resolver.ICtx.empty
      val elab = raise(Stage.Elaborator).givenIn:
        semantics.Elaborator(etl, importer)
      given Ctx = curCtx.nestLocal
      val blk = new syntax.Tree.Block(parsedTrees)
      val (term, newCtx) = elab.topLevel(blk)
      curCtx = newCtx
      val elaboratedTree = term.showAsTree
      val resolver = semantics.Resolver(rtl)(using raise(Stage.Resolver), State)
      curICtx = resolver.traverseBlock(term)(using curICtx)
      val typeCheckOutputs = typeCheck(term)
      // Collect results
      allTokens ++= lexer.tokens.iterator.map(_._1.describe)
      allParsedTrees ++= parsedTrees.iterator.map(_.showAsTree)
      allElaboratedTrees += elaboratedTree
      allResolvedTrees += term.showAsTree
      allTypeCheckOutputs += js.Dynamic.literal(
        firstLineIndex = block._1,
        source = block._2,
        result = typeCheckOutputs
      )
    
    js.Dynamic.literal(
      lexer = js.Dynamic.literal(
        tokens = allTokens.toJSArray,
        diagnostics = getDiagnosticsHTML(Stage.Lexer),
      ),
      parser = js.Dynamic.literal(
        trees = allParsedTrees.toJSArray,
        diagnostics = getDiagnosticsHTML(Stage.Parser),
        traces = getOutput(Stage.Parser)
      ),
      elaborator = js.Dynamic.literal(
        tree = allElaboratedTrees.toJSArray,
        traces = getOutput(Stage.Elaborator),
        diagnostics = getDiagnosticsHTML(Stage.Elaborator),
      ),
      resolver = js.Dynamic.literal(
        tree = allResolvedTrees.toJSArray,
        traces = getOutput(Stage.Resolver),
        diagnostics = getDiagnosticsHTML(Stage.Resolver),
      ),
      typer = js.Dynamic.literal(
        results = allTypeCheckOutputs.toJSArray,
        diagnostics = getDiagnosticsHTML(Stage.Typer),
      )
    )
  
  private val showTypeLatex = false
  
  def typeCheck(term: semantics.Term.Blk): Str =
    given Raise = raise(Stage.Typer)
    val outputLines = Buffer.empty[Str]
    def output(s: Str): Unit = outputLines += s
    val typer = Typer()
    given NamingCtx = NamingCtx(true)
    given InferenceCtx = InferenceCtx(None, Map.empty)
    val ctrm = typer.fromTerm(term)
    output("Parsed Core: " + ctrm.show)
    typer.checkWellFormed(ctrm)
    val (ty, cons_) = typer.inferType(ctrm)
    val cons = cons_ ++ (Constraint(QuantType.Base(ty), NegType.Force(true), Nil) :: Nil)
    output("Inferred: " + (if showTypeLatex then ty.showAsTypeLatex else ty.showAsType))
    output("As term: " + ty.showAsTerm)

    if showTypeLatex then
      output("|>\n" + cons.map(s => s match
        case c: Constraint => c.showLatex(0)
        case (al: TypeVar, _) => al.showLatex
      ).mkString("\n"))
    else
      output("|> " + cons.map(s => s match
        case c: Constraint => c.show
        case (al: TypeVar, _) => al.show
      ).mkString(", "))

    var solver = CtxSolver(cons)
    var fuel = 100
    var iter = 0
    def printBounds = 
      // print bounds
      val ubs = solver.upperBounds.valuesIterator.map(_.size).sum
      val lbs = solver.lowerBounds.valuesIterator.map(_.size).sum
      val bounded = solver.upperBounds.keySet ++ solver.lowerBounds.keySet
      if ubs > 0 then
        output("-------- UBS --------")
      for al <- bounded do
        for ((_, s), ty) <- solver.upperBounds.getOrElse(al, Map.empty[(NegType, Set[Mark]), NegType]) do
          val ss = (if !s.isEmpty then s.map(m => f"m${m.uid}").mkString("[", ",","]") else "")
          if showTypeLatex then
            output(s"${al.showLatex} $$\\leq$$ ${ty.showAsTypeLatex}")
          else
            output(s"${al.show} ≤^${ss} ${ty.showAsType}")
      if lbs > 0 then
        output("-------- LBS --------")
      for al <- bounded do
        for ((_, s), ty) <- solver.lowerBounds.getOrElse(al, Map.empty[(QuantType, Set[Mark]), QuantType]) do
          val ss = (if !s.isEmpty then s.map(m => f"m${m.uid}").mkString("[", ",","]") else "")
          if showTypeLatex then
            output(s"${al.showLatex} $$\\geq$$ ${ty.showAsTypeLatex}")
          else
            output(s"${al.show} ≥^${ss} ${ty.showAsType}")
      if ubs + lbs > 0 then
        output("---------------------")
    while iter < fuel && !solver.unresolved.isEmpty do
      iter += 1
      output(s"====== (${iter}) ======")
      printBounds
      if showTypeLatex then
        output(s"Front:\n${solver.showFrontLatex}")
      else
        output(s"Front: ${solver.showFront}")
      val (rule, newResolved, newCons) = solver.step
      output(s"Rule: ${rule}")
      for con <- newCons do
        if showTypeLatex then
          output(s"|>\n${con.showLatex(0)}")
        else
          output(s"|> ${con.show}")
      if iter == fuel then
        output(s"==== Out of fuel ====")
      if rule == "C-Err" then
        iter = fuel
      if rule.startsWith("C-Forall") then
        for (key, value) <- solver.quantCache.iterator do
          val mrks = key.iterator.map(m => s"m${m.uid}").mkString(",")
          output(s"[${mrks}]")
          output(s"  -> ${value.showAsType}")
      output(s"Remaining: ${solver.unresolved.size}")

    val lBounds = solver.lowerBounds.toList.flatMap:
      case (v, lb) => lb.toList.map((k, l) => Constraint(l, NegType.Var(v), Nil))
    val uBounds = solver.upperBounds.toList.flatMap:
      case (v, ub) => ub.toList.map((k, u) => Constraint(QuantType.fromVar(v), u, Nil))
    val finalType = typer.wrap((ty, lBounds ++ uBounds))

    if iter == fuel then
      output(s"====== Remaining ======")
      for elem <- solver.unresolved do elem match
        case (al: TypeVar, _) => output(s"${if showTypeLatex then al.showLatex else al.show}")
        case c : Constraint => output(s"${if showTypeLatex then c.showLatex(0) else c.show}")
    else
      output(s"====== Final ======")
      output(s"------ base type ------")
      output(s"${(if showTypeLatex then ty.showAsTypeLatex else ty.showAsType)}")
      printBounds
    
    outputLines.mkString("\n")
  
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
  
  private val htmlLineBreak = "<br />"
end Main
