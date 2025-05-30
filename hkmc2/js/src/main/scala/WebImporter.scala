package hkmc2

import mlscript.utils.*, shorthands.*
import semantics.{Import, Importer}
import collection.mutable.Map as MutMap
import semantics.Elaborator.{Ctx, Mode, State}
import hkmc2.Message.MessageContext
import utils.TL

class WebImporter(using tl: TL)(using State, Raise) extends Importer:
  import WebImporter.*
  
  val prelude = Ctx.empty
  
  def importPath(path: Str): Import =
    import syntax.*, semantics.*
    
    val file = path
    val baseName =
      val i = path.lastIndexOf('/')
      if i >= 0 then path.drop(i + 1) else path
    val ext =
      val i = baseName.lastIndexOf('.')
      if i >= 0 then baseName.drop(i + 1) else ""
    val id = new syntax.Tree.Ident(baseName) // TODO loc
    
    lazy val sym = TermSymbol(LetBind, N, id)
    
    if path.startsWith(".") || path.startsWith("/") then // leave alone imports like "fs"
      // log(s"importing $file")
      
      val nme = baseName
      val id = new syntax.Tree.Ident(nme) // TODO loc
      
      ext match
      
      case "mjs" | "js" =>
        Import(sym, file.toString) // TODO: fill in the js file
        
      case "mls" =>
        
        fileNameSourceMap.get(baseName) match
          case Some(block -> optMjsFile) =>
            val fph = new FastParseHelpers(block)
            val origin = Origin(file.toString, 0, fph)
            
            val sym = tl.trace(s">>> Importing $file"):
              
              // TODO add parser option to omit internal impls
              
              val lexer = new syntax.Lexer(origin, dbg = tl.doTrace)
              val tokens = lexer.bracketedTokens
              val rules = syntax.ParseRules()
              val p = new syntax.Parser(origin, tokens, rules, raise, dbg = tl.doTrace):
                def doPrintDbg(msg: => Str): Unit =
                  // if dbg then output(msg)
                  if dbg then tl.log(msg)
              val res = p.parseAll(p.block(allowNewlines = true))
              val resBlk = new syntax.Tree.Block(res)
              
              given Elaborator.Ctx = prelude.copy(mode = Mode.Light).nestLocal
              val elab = Elaborator(tl, this)
              elab.importFrom(resBlk)
              
              resBlk.definedSymbols.find(_._1 === nme) match
              case Some(nme -> sym) => sym
              case None => lastWords(s"File $file does not define a symbol named $nme")
            
            Import(sym, "")
          case None =>
            raise(ErrorReport(msg"Source file $file not found" -> N :: Nil))
            Import(sym, "")
        
      case _ =>
        raise(ErrorReport(msg"Unsupported file extension: ${ext}" -> N :: Nil))
        Import(sym, "")
      
    else
      Import(sym, path)

object WebImporter:
  val fileNameSourceMap: MutMap[Str, (Str, Opt[Str])] = MutMap.empty
  
  fileNameSourceMap += "Rendering.mls" -> ("""
module Rendering with ...


fun pass1(f)(...xs) = f(xs.0)
fun pass2(f)(...xs) = f(xs.0, xs.1)
fun pass3(f)(...xs) = f(xs.0, xs.1, xs.2)

fun passing(f, ...args) = f.bind(null, ...args)

fun map(f)(...xs) = xs.map(pass1(f))

fun fold(f)(init, ...rest) =
  let
    i = 0
    len = rest.length
  while i < len do
    set
      init = f(init, rest.at(i))
      i += 1
  init

fun interleave(sep)(...args) =
  if args.length === 0 then [] else...
  let
    res = Array of args.length * 2 - 1
    len = args.length
    i = 0
  while i < len do
    let idx = i * 2
    set
      res.[idx] = args.[i]
      i += 1
    if i < len do set res.[idx + 1] = sep
  res

fun render(arg) = if
  arg is
    undefined then "undefined"
    null      then "null"
    Array     then fold(+)("[", ...interleave(", ")(...map(render)(...arg)), "]")
    Str       then JSON.stringify(arg)
    Set       then fold(+)("Set{", ...interleave(", ")(...map(render)(...arg)), "}")
    Map       then fold(+)("Map{", ...interleave(", ")(...map(render)(...arg)), "}")
    Function  and
      let p = Object.getOwnPropertyDescriptor(arg, "prototype")
      (p is Object and p.("writable")) || (p is undefined) then
        "[function" + (if arg.name is
          ""  then ""
          nme then " " + nme
        ) + "]"
    Object then
      if arg.constructor.name is "Object"
      then
        let es = Object.entries(arg)
        fold(+)("{", ...interleave(", ")(...map(case [k, v] then k + ": " + render(v))(...es)), "}")
      else String(arg)
  let ts = arg.("toString") // not accessing as `arg.toString` to avoid the sanity check
  ts is undefined then "[" + typeof(arg) + "]"
  else ts.call(arg)
""" -> S("""
import runtime from "./Runtime.mjs";
let Rendering1;
(class Rendering {
  static {
    Rendering1 = Rendering;
  }
  static pass1(f) {
    return (...xs) => {
      return runtime.safeCall(f(xs[0]))
    }
  } 
  static pass2(f1) {
    return (...xs) => {
      return runtime.safeCall(f1(xs[0], xs[1]))
    }
  } 
  static pass3(f2) {
    return (...xs) => {
      return runtime.safeCall(f2(xs[0], xs[1], xs[2]))
    }
  } 
  static passing(f3, ...args) {
    return f3.bind(null, ...args)
  } 
  static map(f4) {
    return (...xs) => {
      let tmp;
      tmp = Rendering.pass1(f4);
      return runtime.safeCall(xs.map(tmp))
    }
  } 
  static fold(f5) {
    return (init, ...rest) => {
      let i, len, scrut, tmp, tmp1, tmp2, tmp3;
      i = 0;
      len = rest.length;
      tmp4: while (true) {
        scrut = i < len;
        if (scrut === true) {
          tmp = runtime.safeCall(rest.at(i));
          tmp1 = runtime.safeCall(f5(init, tmp));
          init = tmp1;
          tmp2 = i + 1;
          i = tmp2;
          tmp3 = runtime.Unit;
          continue tmp4;
        } else {
          tmp3 = runtime.Unit;
        }
        break;
      }
      return init
    }
  } 
  static interleave(sep) {
    return (...args1) => {
      let res, len, i, scrut, idx, scrut1, scrut2, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7;
      scrut2 = args1.length === 0;
      if (scrut2 === true) {
        return []
      } else {
        tmp = args1.length * 2;
        tmp1 = tmp - 1;
        tmp2 = globalThis.Array(tmp1);
        res = tmp2;
        len = args1.length;
        i = 0;
        tmp8: while (true) {
          scrut = i < len;
          if (scrut === true) {
            tmp3 = i * 2;
            idx = tmp3;
            res[idx] = args1[i];
            tmp4 = i + 1;
            i = tmp4;
            scrut1 = i < len;
            if (scrut1 === true) {
              tmp5 = idx + 1;
              res[tmp5] = sep;
              tmp6 = runtime.Unit;
            } else {
              tmp6 = runtime.Unit;
            }
            tmp7 = tmp6;
            continue tmp8;
          } else {
            tmp7 = runtime.Unit;
          }
          break;
        }
        return res
      }
    }
  } 
  static render(arg) {
    let ts, scrut, es, p, scrut1, scrut2, scrut3, nme, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, tmp20, tmp21, tmp22, tmp23, tmp24, tmp25, tmp26, tmp27, tmp28, tmp29, tmp30, tmp31, tmp32, tmp33, tmp34, tmp35, tmp36, lambda, lambda1, lambda2, lambda3, lambda4, lambda5, lambda6;
    if (arg === undefined) {
      return "undefined"
    } else if (arg === null) {
      return "null"
    } else if (arg instanceof globalThis.Array) {
      lambda = (undefined, function (arg1, arg2) {
        return arg1 + arg2
      });
      tmp = Rendering.fold(lambda);
      tmp1 = Rendering.interleave(", ");
      tmp2 = Rendering.map(Rendering.render);
      tmp3 = runtime.safeCall(tmp2(...arg));
      tmp4 = runtime.safeCall(tmp1(...tmp3));
      return runtime.safeCall(tmp("[", ...tmp4, "]"))
    } else if (typeof arg === 'string') {
      return runtime.safeCall(globalThis.JSON.stringify(arg))
    } else if (arg instanceof globalThis.Set) {
      lambda1 = (undefined, function (arg1, arg2) {
        return arg1 + arg2
      });
      tmp5 = Rendering.fold(lambda1);
      tmp6 = Rendering.interleave(", ");
      tmp7 = Rendering.map(Rendering.render);
      tmp8 = runtime.safeCall(tmp7(...arg));
      tmp9 = runtime.safeCall(tmp6(...tmp8));
      return runtime.safeCall(tmp5("Set{", ...tmp9, "}"))
    } else if (arg instanceof globalThis.Map) {
      lambda2 = (undefined, function (arg1, arg2) {
        return arg1 + arg2
      });
      tmp10 = Rendering.fold(lambda2);
      tmp11 = Rendering.interleave(", ");
      tmp12 = Rendering.map(Rendering.render);
      tmp13 = runtime.safeCall(tmp12(...arg));
      tmp14 = runtime.safeCall(tmp11(...tmp13));
      return runtime.safeCall(tmp10("Map{", ...tmp14, "}"))
    } else if (arg instanceof globalThis.Function) {
      p = globalThis.Object.getOwnPropertyDescriptor(arg, "prototype");
      if (p instanceof globalThis.Object) {
        scrut1 = p["writable"];
        if (scrut1 === true) {
          tmp15 = true;
        } else {
          tmp15 = false;
        }
      } else {
        tmp15 = false;
      }
      if (p === undefined) {
        tmp16 = true;
      } else {
        tmp16 = false;
      }
      scrut2 = tmp15 || tmp16;
      if (scrut2 === true) {
        scrut3 = arg.name;
        if (scrut3 === "") {
          tmp17 = "";
        } else {
          nme = scrut3;
          tmp17 = " " + nme;
        }
        tmp18 = "[function" + tmp17;
        return tmp18 + "]"
      } else {
        if (arg instanceof globalThis.Object) {
          scrut = arg.constructor.name;
          if (scrut === "Object") {
            tmp19 = runtime.safeCall(globalThis.Object.entries(arg));
            es = tmp19;
            lambda3 = (undefined, function (arg1, arg2) {
              return arg1 + arg2
            });
            tmp20 = Rendering.fold(lambda3);
            tmp21 = Rendering.interleave(", ");
            lambda4 = (undefined, function (caseScrut) {
              let first1, first0, k, v, tmp37, tmp38;
              if (globalThis.Array.isArray(caseScrut) && caseScrut.length === 2) {
                first0 = caseScrut[0];
                first1 = caseScrut[1];
                k = first0;
                v = first1;
                tmp37 = k + ": ";
                tmp38 = Rendering.render(v);
                return tmp37 + tmp38
              } else {
                throw new globalThis.Error("match error");
              }
            });
            tmp22 = lambda4;
            tmp23 = Rendering.map(tmp22);
            tmp24 = runtime.safeCall(tmp23(...es));
            tmp25 = runtime.safeCall(tmp21(...tmp24));
            return runtime.safeCall(tmp20("{", ...tmp25, "}"))
          } else {
            return globalThis.String(arg)
          }
        } else {
          ts = arg["toString"];
          if (ts === undefined) {
            tmp26 = typeof arg;
            tmp27 = "[" + tmp26;
            return tmp27 + "]"
          } else {
            return runtime.safeCall(ts.call(arg))
          }
        }
      }
    } else if (arg instanceof globalThis.Object) {
      scrut = arg.constructor.name;
      if (scrut === "Object") {
        tmp28 = runtime.safeCall(globalThis.Object.entries(arg));
        es = tmp28;
        lambda5 = (undefined, function (arg1, arg2) {
          return arg1 + arg2
        });
        tmp29 = Rendering.fold(lambda5);
        tmp30 = Rendering.interleave(", ");
        lambda6 = (undefined, function (caseScrut) {
          let first1, first0, k, v, tmp37, tmp38;
          if (globalThis.Array.isArray(caseScrut) && caseScrut.length === 2) {
            first0 = caseScrut[0];
            first1 = caseScrut[1];
            k = first0;
            v = first1;
            tmp37 = k + ": ";
            tmp38 = Rendering.render(v);
            return tmp37 + tmp38
          } else {
            throw new globalThis.Error("match error");
          }
        });
        tmp31 = lambda6;
        tmp32 = Rendering.map(tmp31);
        tmp33 = runtime.safeCall(tmp32(...es));
        tmp34 = runtime.safeCall(tmp30(...tmp33));
        return runtime.safeCall(tmp29("{", ...tmp34, "}"))
      } else {
        return globalThis.String(arg)
      }
    } else {
      ts = arg["toString"];
      if (ts === undefined) {
        tmp35 = typeof arg;
        tmp36 = "[" + tmp35;
        return tmp36 + "]"
      } else {
        return runtime.safeCall(ts.call(arg))
      }
    }
  }
  static toString() { return "Rendering"; }
});
let Rendering = Rendering1; export default Rendering;
"""))
  
  fileNameSourceMap += "Runtime.mls" -> ("""
import "./RuntimeJS.mjs"
import "./Rendering.mls"


module Runtime with ...


object Unit with
  fun toString() = "()"


fun unreachable = throw Error("unreachable")

fun checkArgs(functionName, expected, isUB, got) =
  if got < expected || isUB && got > expected do
    let name = if functionName.length > 0 then " '" + functionName + "'" else ""
    // throw globalThis.Error("Function" + name + " expected "
    //   + (if isUB then "" else "at least ")
    //   + expected
    //   + " argument(s) but got " + got)
    throw Error of "Function" + name + " expected "
      + (if isUB then "" else "at least " )
      + expected + " argument"
      + (if expected === 1 then "" else "s")
      + " but got " + got

fun safeCall(x) =
  if x is undefined then Unit else x

fun checkCall(x) =
  if x is undefined
  then throw Error("MLscript call unexpectedly returned `undefined`, the forbidden value.")
  else x

fun deboundMethod(mtdName, clsName) =
  throw Error of
    "[debinding error] Method '" + mtdName + "' of class '" + clsName + "' was accessed without being called."


val try_catch = RuntimeJS.try_catch

class EffectHandle(_reified) with
  val reified = _reified
  fun resumeWith(value) =
    Runtime.try(() => resume(reified.contTrace)(value))
  fun raise() =
    topLevelEffect(reified, false)

fun try(f) =
  let res = f()
  if res is EffectSig then EffectHandle(res) else res


// For `pattern` definitions
data class MatchResult(captures)
data class MatchFailure(errors)

// For pattern matching on tuples
module Tuple with
  fun slice(xs, i, j) =
    // * This is more robust than `xs.slice(i, xs.length - j)`
    // * as it is not affected by users redefining `slice`
    globalThis.Array.prototype.slice.call(xs, i, xs.length - j)

  fun get(xs, i) =
    // * Contrary to `xs.[i]`, this supports negative indices (Python-style)
    if i >= xs.length then
      throw RangeError("Tuple.get: index out of bounds")
    else globalThis.Array.prototype.at.call(xs, i)

module Str with
  fun startsWith(string, prefix) = string.startsWith(prefix)

  fun get(string, i) =
    if i >= string.length then
      throw RangeError("Str.get: index out of bounds")
    else string.at(i)

  fun drop(string, n) = string.slice(n)

// Re-export rendering functions
val render = Rendering.render

fun printRaw(x) = console.log(render(x))

// TraceLogger

module TraceLogger with
  mut val enabled = false
  mut val indentLvl = 0
  fun indent() =
    if enabled then
      let prev = indentLvl
      set indentLvl = prev + 1
      prev
    else ()
  fun resetIndent(n) =
    if enabled then
      set indentLvl = n
    else ()
  fun log(msg) =
    if enabled then
      console.log("| ".repeat(indentLvl) + msg.replaceAll("\n", "\n" + "  ".repeat(indentLvl)))
    else ()

// Private definitions for algebraic effects

object FatalEffect
object PrintStackEffect

data abstract class FunctionContFrame(next) with
  fun resume(value)
data class HandlerContFrame(next, nextHandler, handler)

data class ContTrace(next, last, nextHandler, lastHandler, resumed)
data class EffectSig(contTrace, handler, handlerFun)

class NonLocalReturn with
  fun ret(value)

data class FnLocalsInfo(fnName, locals)
data class LocalVarInfo(localName, value)


fun raisePrintStackEffect(showLocals) =
  mkEffect(PrintStackEffect, showLocals)

fun topLevelEffect(tr, debug) =
  while tr.handler === PrintStackEffect do
    console.log(showStackTrace("Stack Trace:", tr, debug, tr.handlerFun))
    set tr = resume(tr.contTrace)(())
  if tr is EffectSig then
    throw showStackTrace("Error: Unhandled effect " + tr.handler.constructor.name, tr, debug, false)
  else
    tr

fun showStackTrace(header, tr, debug, showLocals) =
  let
    msg = header
    curHandler = tr.contTrace
    atTail = true
  if debug do
    while curHandler !== null do
      let cur = curHandler.next
      while cur !== null do
        let locals = cur.getLocals
        let curLocals = locals.at(locals.length - 1)
        let loc = cur.getLoc
        let loc = if loc is null then "pc=" + cur.pc else loc
        let localsMsg = if showLocals and curLocals.locals.length > 0 then
          " with locals: " + curLocals.locals.map(l => l.localName + "=" + Rendering.render(l.value)).join(", ")
        else
          ""
        set
          msg += "\n\tat " + curLocals.fnName + " (" + loc + ")"
          msg += localsMsg
          cur = cur.next
          atTail = false
      set curHandler = curHandler.nextHandler
      if curHandler !== null do
        set
          msg += "\n\twith handler " + curHandler.handler.constructor.name
          atTail = false
    if atTail do
      set msg += "\n\tat tail position"
  msg

fun showFunctionContChain(cont, hl, vis, reps) =
  if cont is FunctionContFrame then
    let result = cont.constructor.name + "(pc=" + cont.pc
    hl.forEach((m, marker) => if m.has(cont) do set result += ", " + marker)
    if vis.has(cont) then
      set reps = reps + 1
      if reps > 10 do
        throw Error("10 repeated continuation frame (loop?)")
      set result += ", REPEAT"
    else
      vis.add(cont)
    result + ") -> " + showFunctionContChain(cont.next, hl, vis, reps)
  else if cont === null then
    "(null)"
  else
    "(NOT CONT)"

fun showHandlerContChain(cont, hl, vis, reps) =
  if cont is HandlerContFrame then
    let result = cont.handler.constructor.name
    hl.forEach((m, marker) => if m.has(cont) do set result += ", " + marker)
    if vis.has(cont) then
      set reps = reps + 1
      if reps > 10 do
        throw Error("10 repeated continuation frame (loop?)")
      set result += ", REPEAT"
    else
      vis.add(cont)
    result + " -> " + showFunctionContChain(cont.next, hl, vis, reps)
  else if cont === null then
    "(null)"
  else
    "(NOT HANDLER CONT)"

fun debugCont(cont) = console.log(showFunctionContChain(cont, new Map(), new Set(), 0))
fun debugHandler(cont) = console.log(showHandlerContChain(cont, new Map(), new Set(), 0))

fun debugContTrace(contTrace) =
  if contTrace is ContTrace then
    console.log("resumed: ", contTrace.resumed)
    if contTrace.last === contTrace do
      console.log("<last is self>")
    if contTrace.lastHandler === contTrace do
      console.log("<lastHandler is self>")
    let vis = new Set()
    let hl = new Map()
    hl.set("last", new Set([contTrace.last]))
    hl.set("last-handler", new Set([contTrace.lastHandler]))
    console.log(showFunctionContChain(contTrace.next, hl, vis, 0))
    let cur = contTrace.nextHandler
    while cur !== null do
      console.log(showHandlerContChain(cur, hl, vis, 0))
      set cur = cur.nextHandler
    console.log()
  else
    console.log("Not a cont trace:")
    console.log(contTrace)

fun debugEff(eff) =
  if eff is EffectSig then
    console.log("Debug EffectSig:")
    console.log("handler: ", eff.handler.constructor.name)
    console.log("handlerFun: ", eff.handlerFun)
    debugContTrace(eff.contTrace)
  else
    console.log("Not an effect:")
    console.log(eff)

// runtime implementations
fun mkEffect(handler, handlerFun) =
  let res = new EffectSig(new ContTrace(null, null, null, null, false), handler, handlerFun)
  set
    res.contTrace.last = res.contTrace
    res.contTrace.lastHandler = res.contTrace
  res

fun handleBlockImpl(cur, handler) =
  let handlerFrame = new HandlerContFrame(null, null, handler)
  set
    cur.contTrace.lastHandler.nextHandler = handlerFrame
    cur.contTrace.lastHandler = handlerFrame
    cur.contTrace.last = handlerFrame
  handleEffects(cur)

fun enterHandleBlock(handler, body) =
  let cur = body()
  if cur is EffectSig then
    handleBlockImpl(cur, handler)
  else
    cur

fun handleEffects(cur) =
  while cur is
    EffectSig then
      let nxt = handleEffect(cur)
      if cur === nxt then
        return cur
      else
        set cur = nxt
    else
      return cur

// return either new effect, final result or the same continuation if there is no handler
fun handleEffect(cur) =
  // debugEff(cur)
  // find the handle block corresponding to the current effect
  let prevHandlerFrame = cur.contTrace
  while prevHandlerFrame.nextHandler !== null and prevHandlerFrame.nextHandler.handler !== cur.handler do
    set prevHandlerFrame = prevHandlerFrame.nextHandler

  // no matching handle block
  if prevHandlerFrame.nextHandler === null do
    return cur

  // the matching handle block
  let handlerFrame = prevHandlerFrame.nextHandler

  // unlink and save frames
  let saved = new ContTrace(
    handlerFrame.next,
    cur.contTrace.last,
    handlerFrame.nextHandler,
    cur.contTrace.lastHandler,
    false
  )
  set
    cur.contTrace.last = handlerFrame
    cur.contTrace.lastHandler = handlerFrame
    handlerFrame.next = null
    handlerFrame.nextHandler = null

  // handle the effect
  set cur = cur.handlerFun(resume(cur.contTrace))
  if cur is EffectSig then
    // relink the saved frames
    if saved.next !== null do
      set
        cur.contTrace.last.next = saved.next
        cur.contTrace.last = saved.last
    if saved.nextHandler !== null do
      set
        cur.contTrace.lastHandler.nextHandler = saved.nextHandler
        cur.contTrace.lastHandler = saved.lastHandler
    cur
  else
    // resume the unlinked handle blocks
    resumeContTrace(saved, cur)

fun resume(contTrace)(value) =
  if contTrace.resumed do
    throw Error("Multiple resumption")
  set contTrace.resumed = true
  handleEffects(resumeContTrace(contTrace, value))

fun resumeContTrace(contTrace, value) =
  let cont = contTrace.next
  let handlerCont = contTrace.nextHandler
  while
    cont is FunctionContFrame then
      set value = cont.resume(value)
      if value is EffectSig then
        set
          value.contTrace.last.next = cont.next
          value.contTrace.lastHandler.nextHandler = handlerCont
        if contTrace.last !== cont do
          set value.contTrace.last = contTrace.last
        if handlerCont !== null do
          set value.contTrace.lastHandler = contTrace.lastHandler
        return value
      else
        set cont = cont.next
    handlerCont is HandlerContFrame then
      set cont = handlerCont.next
      set handlerCont = handlerCont.nextHandler
    else
      return value

// stack safety
mut val stackLimit = 0 // How deep the stack can go before heapifying the stack
mut val stackDepth = 0 // Tracks the virtual + real stack depth
mut val stackOffset = 0 // How much to offset stackDepth by to get the true stack depth (i.e. the virtual depth)
mut val stackHandler = null
mut val stackResume = null

object StackDelayHandler with
  fun delay() = mkEffect of this, k =>
    set stackResume = k

fun checkDepth() =
  if stackDepth - stackOffset >= stackLimit && stackHandler !== null then
    // this is a tail call to effectful function
    stackHandler.delay()
  else
    ()

fun resetDepth(tmp, curDepth) =
  set stackDepth = curDepth
  if curDepth < stackOffset do
    set stackOffset = curDepth
  tmp

fun runStackSafe(limit, f) =
  set
    stackLimit = limit
    stackDepth = 1
    stackOffset = 0
    stackHandler = StackDelayHandler
  let result = enterHandleBlock(StackDelayHandler, f)
  while stackResume !== null do
    let saved = stackResume
    set
      stackResume = null
      stackOffset = stackDepth
      result = saved()
  set
    stackLimit = 0
    stackDepth = 0
    stackOffset = 0
    stackHandler = null
  result
""" -> S("""
import runtime from "./Runtime.mjs";
import RuntimeJS from "./RuntimeJS.mjs";
import Rendering from "./Rendering.mjs";
let Runtime1;
(class Runtime {
  static {
    Runtime1 = Runtime;
    const Unit$class = class Unit {
      constructor() {}
      toString() {
        return "()"
      }
    };
    this.Unit = new Unit$class;
    this.Unit.class = Unit$class;
    this.try_catch = RuntimeJS.try_catch;
    this.EffectHandle = function EffectHandle(_reified1) {
      return new EffectHandle.class(_reified1);
    };
    this.EffectHandle.class = class EffectHandle {
      #_reified;
      constructor(_reified) {
        this.#_reified = _reified;
        this.reified = this.#_reified;
      }
      resumeWith(value) {
        let lambda;
        const this$EffectHandle = this;
        lambda = (undefined, function () {
          let tmp;
          tmp = Runtime.resume(this$EffectHandle.reified.contTrace);
          return runtime.safeCall(tmp(value))
        });
        return Runtime1.try(lambda)
      } 
      raise() {
        return Runtime.topLevelEffect(this.reified, false)
      }
      toString() { return "EffectHandle(" + "" + ")"; }
    };
    this.MatchResult = function MatchResult(captures1) {
      return new MatchResult.class(captures1);
    };
    this.MatchResult.class = class MatchResult {
      constructor(captures) {
        this.captures = captures;
      }
      toString() { return "MatchResult(" + runtime.render(this.captures) + ")"; }
    };
    this.MatchFailure = function MatchFailure(errors1) {
      return new MatchFailure.class(errors1);
    };
    this.MatchFailure.class = class MatchFailure {
      constructor(errors) {
        this.errors = errors;
      }
      toString() { return "MatchFailure(" + runtime.render(this.errors) + ")"; }
    };
    (class Tuple {
      static {
        Runtime.Tuple = Tuple;
      }
      static slice(xs, i, j) {
        let tmp;
        tmp = xs.length - j;
        return runtime.safeCall(globalThis.Array.prototype.slice.call(xs, i, tmp))
      } 
      static get(xs1, i1) {
        let scrut;
        scrut = i1 >= xs1.length;
        if (scrut === true) {
          throw globalThis.RangeError("Tuple.get: index out of bounds");
        } else {
          return globalThis.Array.prototype.at.call(xs1, i1)
        }
      }
      static toString() { return "Tuple"; }
    });
    (class Str {
      static {
        Runtime.Str = Str;
      }
      static startsWith(string, prefix) {
        return runtime.safeCall(string.startsWith(prefix))
      } 
      static get(string1, i) {
        let scrut;
        scrut = i >= string1.length;
        if (scrut === true) {
          throw globalThis.RangeError("Str.get: index out of bounds");
        } else {
          return runtime.safeCall(string1.at(i))
        }
      } 
      static drop(string2, n) {
        return runtime.safeCall(string2.slice(n))
      }
      static toString() { return "Str"; }
    });
    this.render = Rendering.render;
    (class TraceLogger {
      static {
        Runtime.TraceLogger = TraceLogger;
        this.enabled = false;
        this.indentLvl = 0;
      }
      static indent() {
        let scrut, prev, tmp;
        scrut = TraceLogger.enabled;
        if (scrut === true) {
          prev = TraceLogger.indentLvl;
          tmp = prev + 1;
          TraceLogger.indentLvl = tmp;
          return prev
        } else {
          return runtime.Unit
        }
      } 
      static resetIndent(n) {
        let scrut;
        scrut = TraceLogger.enabled;
        if (scrut === true) {
          TraceLogger.indentLvl = n;
          return runtime.Unit
        } else {
          return runtime.Unit
        }
      } 
      static log(msg) {
        let scrut, tmp, tmp1, tmp2, tmp3, tmp4;
        scrut = TraceLogger.enabled;
        if (scrut === true) {
          tmp = runtime.safeCall("| ".repeat(TraceLogger.indentLvl));
          tmp1 = runtime.safeCall("  ".repeat(TraceLogger.indentLvl));
          tmp2 = "\n" + tmp1;
          tmp3 = msg.replaceAll("\n", tmp2);
          tmp4 = tmp + tmp3;
          return runtime.safeCall(globalThis.console.log(tmp4))
        } else {
          return runtime.Unit
        }
      }
      static toString() { return "TraceLogger"; }
    });
    const FatalEffect$class = class FatalEffect {
      constructor() {}
      toString() { return "FatalEffect"; }
    };
    this.FatalEffect = new FatalEffect$class;
    this.FatalEffect.class = FatalEffect$class;
    const PrintStackEffect$class = class PrintStackEffect {
      constructor() {}
      toString() { return "PrintStackEffect"; }
    };
    this.PrintStackEffect = new PrintStackEffect$class;
    this.PrintStackEffect.class = PrintStackEffect$class;
    this.FunctionContFrame = function FunctionContFrame(next1) {
      return new FunctionContFrame.class(next1);
    };
    this.FunctionContFrame.class = class FunctionContFrame {
      constructor(next) {
        this.next = next;
      }
      toString() { return "FunctionContFrame(" + runtime.render(this.next) + ")"; }
    };
    this.HandlerContFrame = function HandlerContFrame(next1, nextHandler1, handler1) {
      return new HandlerContFrame.class(next1, nextHandler1, handler1);
    };
    this.HandlerContFrame.class = class HandlerContFrame {
      constructor(next, nextHandler, handler) {
        this.next = next;
        this.nextHandler = nextHandler;
        this.handler = handler;
      }
      toString() { return "HandlerContFrame(" + runtime.render(this.next) + ", " + runtime.render(this.nextHandler) + ", " + runtime.render(this.handler) + ")"; }
    };
    this.ContTrace = function ContTrace(next1, last1, nextHandler1, lastHandler1, resumed1) {
      return new ContTrace.class(next1, last1, nextHandler1, lastHandler1, resumed1);
    };
    this.ContTrace.class = class ContTrace {
      constructor(next, last, nextHandler, lastHandler, resumed) {
        this.next = next;
        this.last = last;
        this.nextHandler = nextHandler;
        this.lastHandler = lastHandler;
        this.resumed = resumed;
      }
      toString() { return "ContTrace(" + runtime.render(this.next) + ", " + runtime.render(this.last) + ", " + runtime.render(this.nextHandler) + ", " + runtime.render(this.lastHandler) + ", " + runtime.render(this.resumed) + ")"; }
    };
    this.EffectSig = function EffectSig(contTrace1, handler1, handlerFun1) {
      return new EffectSig.class(contTrace1, handler1, handlerFun1);
    };
    this.EffectSig.class = class EffectSig {
      constructor(contTrace, handler, handlerFun) {
        this.contTrace = contTrace;
        this.handler = handler;
        this.handlerFun = handlerFun;
      }
      toString() { return "EffectSig(" + runtime.render(this.contTrace) + ", " + runtime.render(this.handler) + ", " + runtime.render(this.handlerFun) + ")"; }
    };
    this.NonLocalReturn = class NonLocalReturn {
      constructor() {}
      toString() { return "NonLocalReturn"; }
    };
    this.FnLocalsInfo = function FnLocalsInfo(fnName1, locals1) {
      return new FnLocalsInfo.class(fnName1, locals1);
    };
    this.FnLocalsInfo.class = class FnLocalsInfo {
      constructor(fnName, locals) {
        this.fnName = fnName;
        this.locals = locals;
      }
      toString() { return "FnLocalsInfo(" + runtime.render(this.fnName) + ", " + runtime.render(this.locals) + ")"; }
    };
    this.LocalVarInfo = function LocalVarInfo(localName1, value1) {
      return new LocalVarInfo.class(localName1, value1);
    };
    this.LocalVarInfo.class = class LocalVarInfo {
      constructor(localName, value) {
        this.localName = localName;
        this.value = value;
      }
      toString() { return "LocalVarInfo(" + runtime.render(this.localName) + ", " + runtime.render(this.value) + ")"; }
    };
    this.stackLimit = 0;
    this.stackDepth = 0;
    this.stackOffset = 0;
    this.stackHandler = null;
    this.stackResume = null;
    const StackDelayHandler$class = class StackDelayHandler {
      constructor() {}
      delay() {
        let lambda;
        lambda = (undefined, function (k) {
          Runtime.stackResume = k;
          return runtime.Unit
        });
        return Runtime.mkEffect(this, lambda)
      }
      toString() { return "StackDelayHandler"; }
    };
    this.StackDelayHandler = new StackDelayHandler$class;
    this.StackDelayHandler.class = StackDelayHandler$class;
  }
  static get unreachable() {
    throw globalThis.Error("unreachable");
  } 
  static checkArgs(functionName, expected, isUB, got) {
    let scrut, name, scrut1, scrut2, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14;
    tmp = got < expected;
    tmp1 = got > expected;
    tmp2 = isUB && tmp1;
    scrut = tmp || tmp2;
    if (scrut === true) {
      scrut1 = functionName.length > 0;
      if (scrut1 === true) {
        tmp3 = " '" + functionName;
        tmp4 = tmp3 + "'";
      } else {
        tmp4 = "";
      }
      name = tmp4;
      tmp5 = "Function" + name;
      tmp6 = tmp5 + " expected ";
      if (isUB === true) {
        tmp7 = "";
      } else {
        tmp7 = "at least ";
      }
      tmp8 = tmp6 + tmp7;
      tmp9 = tmp8 + expected;
      tmp10 = tmp9 + " argument";
      scrut2 = expected === 1;
      if (scrut2 === true) {
        tmp11 = "";
      } else {
        tmp11 = "s";
      }
      tmp12 = tmp10 + tmp11;
      tmp13 = tmp12 + " but got ";
      tmp14 = tmp13 + got;
      throw globalThis.Error(tmp14);
    } else {
      return runtime.Unit
    }
  } 
  static safeCall(x) {
    if (x === undefined) {
      return Runtime.Unit
    } else {
      return x
    }
  } 
  static checkCall(x1) {
    if (x1 === undefined) {
      throw globalThis.Error("MLscript call unexpectedly returned `undefined`, the forbidden value.");
    } else {
      return x1
    }
  } 
  static deboundMethod(mtdName, clsName) {
    let tmp, tmp1, tmp2, tmp3;
    tmp = "[debinding error] Method '" + mtdName;
    tmp1 = tmp + "' of class '";
    tmp2 = tmp1 + clsName;
    tmp3 = tmp2 + "' was accessed without being called.";
    throw globalThis.Error(tmp3);
  } 
  static try(f) {
    let res, tmp;
    tmp = runtime.safeCall(f());
    res = tmp;
    if (res instanceof Runtime.EffectSig.class) {
      return runtime.safeCall(Runtime.EffectHandle(res))
    } else {
      return res
    }
  } 
  static printRaw(x2) {
    let tmp;
    tmp = runtime.safeCall(Runtime.render(x2));
    return runtime.safeCall(globalThis.console.log(tmp))
  } 
  static raisePrintStackEffect(showLocals) {
    return Runtime.mkEffect(Runtime.PrintStackEffect, showLocals)
  } 
  static topLevelEffect(tr, debug) {
    let scrut, tmp, tmp1, tmp2, tmp3, tmp4, tmp5;
    tmp6: while (true) {
      scrut = tr.handler === Runtime.PrintStackEffect;
      if (scrut === true) {
        tmp = Runtime.showStackTrace("Stack Trace:", tr, debug, tr.handlerFun);
        tmp1 = runtime.safeCall(globalThis.console.log(tmp));
        tmp2 = Runtime.resume(tr.contTrace);
        tmp3 = runtime.safeCall(tmp2(runtime.Unit));
        tr = tmp3;
        tmp4 = runtime.Unit;
        continue tmp6;
      } else {
        tmp4 = runtime.Unit;
      }
      break;
    }
    if (tr instanceof Runtime.EffectSig.class) {
      tmp5 = "Error: Unhandled effect " + tr.handler.constructor.name;
      throw Runtime.showStackTrace(tmp5, tr, debug, false);
    } else {
      return tr
    }
  } 
  static showStackTrace(header, tr1, debug1, showLocals1) {
    let msg, curHandler, atTail, scrut, cur, scrut1, locals, curLocals, loc, loc1, localsMsg, scrut2, scrut3, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15, tmp16, tmp17, tmp18, tmp19, lambda;
    msg = header;
    curHandler = tr1.contTrace;
    atTail = true;
    if (debug1 === true) {
      tmp20: while (true) {
        scrut = curHandler !== null;
        if (scrut === true) {
          cur = curHandler.next;
          tmp21: while (true) {
            scrut1 = cur !== null;
            if (scrut1 === true) {
              locals = cur.getLocals;
              tmp = locals.length - 1;
              tmp1 = runtime.safeCall(locals.at(tmp));
              curLocals = tmp1;
              loc = cur.getLoc;
              if (loc === null) {
                tmp2 = "pc=" + cur.pc;
              } else {
                tmp2 = loc;
              }
              loc1 = tmp2;
              if (showLocals1 === true) {
                scrut2 = curLocals.locals.length > 0;
                if (scrut2 === true) {
                  lambda = (undefined, function (l) {
                    let tmp22, tmp23;
                    tmp22 = l.localName + "=";
                    tmp23 = Rendering.render(l.value);
                    return tmp22 + tmp23
                  });
                  tmp3 = runtime.safeCall(curLocals.locals.map(lambda));
                  tmp4 = runtime.safeCall(tmp3.join(", "));
                  tmp5 = " with locals: " + tmp4;
                } else {
                  tmp5 = "";
                }
              } else {
                tmp5 = "";
              }
              localsMsg = tmp5;
              tmp6 = "\n\tat " + curLocals.fnName;
              tmp7 = tmp6 + " (";
              tmp8 = tmp7 + loc1;
              tmp9 = tmp8 + ")";
              tmp10 = msg + tmp9;
              msg = tmp10;
              tmp11 = msg + localsMsg;
              msg = tmp11;
              cur = cur.next;
              atTail = false;
              tmp12 = runtime.Unit;
              continue tmp21;
            } else {
              tmp12 = runtime.Unit;
            }
            break;
          }
          curHandler = curHandler.nextHandler;
          scrut3 = curHandler !== null;
          if (scrut3 === true) {
            tmp13 = "\n\twith handler " + curHandler.handler.constructor.name;
            tmp14 = msg + tmp13;
            msg = tmp14;
            atTail = false;
            tmp15 = runtime.Unit;
          } else {
            tmp15 = runtime.Unit;
          }
          tmp16 = tmp15;
          continue tmp20;
        } else {
          tmp16 = runtime.Unit;
        }
        break;
      }
      if (atTail === true) {
        tmp17 = msg + "\n\tat tail position";
        msg = tmp17;
        tmp18 = runtime.Unit;
      } else {
        tmp18 = runtime.Unit;
      }
      tmp19 = tmp18;
    } else {
      tmp19 = runtime.Unit;
    }
    return msg
  } 
  static showFunctionContChain(cont, hl, vis, reps) {
    let scrut, result, scrut1, scrut2, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, lambda;
    if (cont instanceof Runtime.FunctionContFrame.class) {
      tmp = cont.constructor.name + "(pc=";
      tmp1 = tmp + cont.pc;
      result = tmp1;
      lambda = (undefined, function (m, marker) {
        let scrut3, tmp10, tmp11;
        scrut3 = runtime.safeCall(m.has(cont));
        if (scrut3 === true) {
          tmp10 = ", " + marker;
          tmp11 = result + tmp10;
          result = tmp11;
          return runtime.Unit
        } else {
          return runtime.Unit
        }
      });
      tmp2 = lambda;
      tmp3 = runtime.safeCall(hl.forEach(tmp2));
      scrut1 = runtime.safeCall(vis.has(cont));
      if (scrut1 === true) {
        tmp4 = reps + 1;
        reps = tmp4;
        scrut2 = reps > 10;
        if (scrut2 === true) {
          throw globalThis.Error("10 repeated continuation frame (loop?)");
        } else {
          tmp5 = runtime.Unit;
        }
        tmp6 = result + ", REPEAT";
        result = tmp6;
        tmp7 = runtime.Unit;
      } else {
        tmp7 = runtime.safeCall(vis.add(cont));
      }
      tmp8 = result + ") -> ";
      tmp9 = Runtime.showFunctionContChain(cont.next, hl, vis, reps);
      return tmp8 + tmp9
    } else {
      scrut = cont === null;
      if (scrut === true) {
        return "(null)"
      } else {
        return "(NOT CONT)"
      }
    }
  } 
  static showHandlerContChain(cont1, hl1, vis1, reps1) {
    let scrut, result, scrut1, scrut2, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, lambda;
    if (cont1 instanceof Runtime.HandlerContFrame.class) {
      result = cont1.handler.constructor.name;
      lambda = (undefined, function (m, marker) {
        let scrut3, tmp8, tmp9;
        scrut3 = runtime.safeCall(m.has(cont1));
        if (scrut3 === true) {
          tmp8 = ", " + marker;
          tmp9 = result + tmp8;
          result = tmp9;
          return runtime.Unit
        } else {
          return runtime.Unit
        }
      });
      tmp = lambda;
      tmp1 = runtime.safeCall(hl1.forEach(tmp));
      scrut1 = runtime.safeCall(vis1.has(cont1));
      if (scrut1 === true) {
        tmp2 = reps1 + 1;
        reps1 = tmp2;
        scrut2 = reps1 > 10;
        if (scrut2 === true) {
          throw globalThis.Error("10 repeated continuation frame (loop?)");
        } else {
          tmp3 = runtime.Unit;
        }
        tmp4 = result + ", REPEAT";
        result = tmp4;
        tmp5 = runtime.Unit;
      } else {
        tmp5 = runtime.safeCall(vis1.add(cont1));
      }
      tmp6 = result + " -> ";
      tmp7 = Runtime.showFunctionContChain(cont1.next, hl1, vis1, reps1);
      return tmp6 + tmp7
    } else {
      scrut = cont1 === null;
      if (scrut === true) {
        return "(null)"
      } else {
        return "(NOT HANDLER CONT)"
      }
    }
  } 
  static debugCont(cont2) {
    let tmp, tmp1, tmp2;
    tmp = new globalThis.Map();
    tmp1 = new globalThis.Set();
    tmp2 = Runtime.showFunctionContChain(cont2, tmp, tmp1, 0);
    return runtime.safeCall(globalThis.console.log(tmp2))
  } 
  static debugHandler(cont3) {
    let tmp, tmp1, tmp2;
    tmp = new globalThis.Map();
    tmp1 = new globalThis.Set();
    tmp2 = Runtime.showHandlerContChain(cont3, tmp, tmp1, 0);
    return runtime.safeCall(globalThis.console.log(tmp2))
  } 
  static debugContTrace(contTrace) {
    let scrut, scrut1, vis2, hl2, cur, scrut2, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14;
    if (contTrace instanceof Runtime.ContTrace.class) {
      tmp = globalThis.console.log("resumed: ", contTrace.resumed);
      scrut = contTrace.last === contTrace;
      if (scrut === true) {
        tmp1 = runtime.safeCall(globalThis.console.log("<last is self>"));
      } else {
        tmp1 = runtime.Unit;
      }
      scrut1 = contTrace.lastHandler === contTrace;
      if (scrut1 === true) {
        tmp2 = runtime.safeCall(globalThis.console.log("<lastHandler is self>"));
      } else {
        tmp2 = runtime.Unit;
      }
      tmp3 = new globalThis.Set();
      vis2 = tmp3;
      tmp4 = new globalThis.Map();
      hl2 = tmp4;
      tmp5 = new globalThis.Set([
        contTrace.last
      ]);
      tmp6 = hl2.set("last", tmp5);
      tmp7 = new globalThis.Set([
        contTrace.lastHandler
      ]);
      tmp8 = hl2.set("last-handler", tmp7);
      tmp9 = Runtime.showFunctionContChain(contTrace.next, hl2, vis2, 0);
      tmp10 = runtime.safeCall(globalThis.console.log(tmp9));
      cur = contTrace.nextHandler;
      tmp15: while (true) {
        scrut2 = cur !== null;
        if (scrut2 === true) {
          tmp11 = Runtime.showHandlerContChain(cur, hl2, vis2, 0);
          tmp12 = runtime.safeCall(globalThis.console.log(tmp11));
          cur = cur.nextHandler;
          tmp13 = runtime.Unit;
          continue tmp15;
        } else {
          tmp13 = runtime.Unit;
        }
        break;
      }
      return runtime.safeCall(globalThis.console.log())
    } else {
      tmp14 = runtime.safeCall(globalThis.console.log("Not a cont trace:"));
      return runtime.safeCall(globalThis.console.log(contTrace))
    }
  } 
  static debugEff(eff) {
    let tmp, tmp1, tmp2, tmp3;
    if (eff instanceof Runtime.EffectSig.class) {
      tmp = runtime.safeCall(globalThis.console.log("Debug EffectSig:"));
      tmp1 = globalThis.console.log("handler: ", eff.handler.constructor.name);
      tmp2 = globalThis.console.log("handlerFun: ", eff.handlerFun);
      return Runtime.debugContTrace(eff.contTrace)
    } else {
      tmp3 = runtime.safeCall(globalThis.console.log("Not an effect:"));
      return runtime.safeCall(globalThis.console.log(eff))
    }
  } 
  static mkEffect(handler, handlerFun) {
    let res, tmp, tmp1;
    tmp = new Runtime.ContTrace.class(null, null, null, null, false);
    tmp1 = new Runtime.EffectSig.class(tmp, handler, handlerFun);
    res = tmp1;
    res.contTrace.last = res.contTrace;
    res.contTrace.lastHandler = res.contTrace;
    return res
  } 
  static handleBlockImpl(cur, handler1) {
    let handlerFrame, tmp;
    tmp = new Runtime.HandlerContFrame.class(null, null, handler1);
    handlerFrame = tmp;
    cur.contTrace.lastHandler.nextHandler = handlerFrame;
    cur.contTrace.lastHandler = handlerFrame;
    cur.contTrace.last = handlerFrame;
    return Runtime.handleEffects(cur)
  } 
  static enterHandleBlock(handler2, body) {
    let cur1, tmp;
    tmp = runtime.safeCall(body());
    cur1 = tmp;
    if (cur1 instanceof Runtime.EffectSig.class) {
      return Runtime.handleBlockImpl(cur1, handler2)
    } else {
      return cur1
    }
  } 
  static handleEffects(cur1) {
    let nxt, scrut, tmp, tmp1, tmp2;
    tmp3: while (true) {
      if (cur1 instanceof Runtime.EffectSig.class) {
        tmp = Runtime.handleEffect(cur1);
        nxt = tmp;
        scrut = cur1 === nxt;
        if (scrut === true) {
          return cur1
        } else {
          cur1 = nxt;
          tmp1 = runtime.Unit;
        }
        tmp2 = tmp1;
        continue tmp3;
      } else {
        return cur1
      }
      break;
    }
    return tmp2
  } 
  static handleEffect(cur2) {
    let prevHandlerFrame, scrut, scrut1, scrut2, handlerFrame, saved, scrut3, scrut4, tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;
    prevHandlerFrame = cur2.contTrace;
    tmp7: while (true) {
      scrut = prevHandlerFrame.nextHandler !== null;
      if (scrut === true) {
        scrut1 = prevHandlerFrame.nextHandler.handler !== cur2.handler;
        if (scrut1 === true) {
          prevHandlerFrame = prevHandlerFrame.nextHandler;
          tmp = runtime.Unit;
          continue tmp7;
        } else {
          tmp = runtime.Unit;
        }
      } else {
        tmp = runtime.Unit;
      }
      break;
    }
    scrut2 = prevHandlerFrame.nextHandler === null;
    if (scrut2 === true) {
      return cur2
    } else {
      tmp1 = runtime.Unit;
    }
    handlerFrame = prevHandlerFrame.nextHandler;
    tmp2 = new Runtime.ContTrace.class(handlerFrame.next, cur2.contTrace.last, handlerFrame.nextHandler, cur2.contTrace.lastHandler, false);
    saved = tmp2;
    cur2.contTrace.last = handlerFrame;
    cur2.contTrace.lastHandler = handlerFrame;
    handlerFrame.next = null;
    handlerFrame.nextHandler = null;
    tmp3 = Runtime.resume(cur2.contTrace);
    tmp4 = runtime.safeCall(cur2.handlerFun(tmp3));
    cur2 = tmp4;
    if (cur2 instanceof Runtime.EffectSig.class) {
      scrut3 = saved.next !== null;
      if (scrut3 === true) {
        cur2.contTrace.last.next = saved.next;
        cur2.contTrace.last = saved.last;
        tmp5 = runtime.Unit;
      } else {
        tmp5 = runtime.Unit;
      }
      scrut4 = saved.nextHandler !== null;
      if (scrut4 === true) {
        cur2.contTrace.lastHandler.nextHandler = saved.nextHandler;
        cur2.contTrace.lastHandler = saved.lastHandler;
        tmp6 = runtime.Unit;
      } else {
        tmp6 = runtime.Unit;
      }
      return cur2
    } else {
      return Runtime.resumeContTrace(saved, cur2)
    }
  } 
  static resume(contTrace1) {
    return (value) => {
      let scrut, tmp, tmp1;
      scrut = contTrace1.resumed;
      if (scrut === true) {
        throw globalThis.Error("Multiple resumption");
      } else {
        tmp = runtime.Unit;
      }
      contTrace1.resumed = true;
      tmp1 = Runtime.resumeContTrace(contTrace1, value);
      return Runtime.handleEffects(tmp1)
    }
  } 
  static resumeContTrace(contTrace2, value) {
    let cont4, handlerCont, scrut, scrut1, tmp, tmp1, tmp2, tmp3, tmp4;
    cont4 = contTrace2.next;
    handlerCont = contTrace2.nextHandler;
    tmp5: while (true) {
      if (cont4 instanceof Runtime.FunctionContFrame.class) {
        tmp = runtime.safeCall(cont4.resume(value));
        value = tmp;
        if (value instanceof Runtime.EffectSig.class) {
          value.contTrace.last.next = cont4.next;
          value.contTrace.lastHandler.nextHandler = handlerCont;
          scrut = contTrace2.last !== cont4;
          if (scrut === true) {
            value.contTrace.last = contTrace2.last;
            tmp1 = runtime.Unit;
          } else {
            tmp1 = runtime.Unit;
          }
          scrut1 = handlerCont !== null;
          if (scrut1 === true) {
            value.contTrace.lastHandler = contTrace2.lastHandler;
            tmp2 = runtime.Unit;
          } else {
            tmp2 = runtime.Unit;
          }
          return value
        } else {
          cont4 = cont4.next;
          tmp3 = runtime.Unit;
        }
        tmp4 = tmp3;
        continue tmp5;
      } else {
        if (handlerCont instanceof Runtime.HandlerContFrame.class) {
          cont4 = handlerCont.next;
          handlerCont = handlerCont.nextHandler;
          tmp4 = runtime.Unit;
          continue tmp5;
        } else {
          return value
        }
      }
      break;
    }
    return tmp4
  } 
  static checkDepth() {
    let scrut, tmp, tmp1, tmp2;
    tmp = Runtime.stackDepth - Runtime.stackOffset;
    tmp1 = tmp >= Runtime.stackLimit;
    tmp2 = Runtime.stackHandler !== null;
    scrut = tmp1 && tmp2;
    if (scrut === true) {
      return runtime.safeCall(Runtime.stackHandler.delay())
    } else {
      return runtime.Unit
    }
  } 
  static resetDepth(tmp, curDepth) {
    let scrut, tmp1;
    Runtime.stackDepth = curDepth;
    scrut = curDepth < Runtime.stackOffset;
    if (scrut === true) {
      Runtime.stackOffset = curDepth;
      tmp1 = runtime.Unit;
    } else {
      tmp1 = runtime.Unit;
    }
    return tmp
  } 
  static runStackSafe(limit, f1) {
    let result, scrut, saved, tmp1, tmp2, tmp3;
    Runtime.stackLimit = limit;
    Runtime.stackDepth = 1;
    Runtime.stackOffset = 0;
    Runtime.stackHandler = Runtime.StackDelayHandler;
    tmp1 = Runtime.enterHandleBlock(Runtime.StackDelayHandler, f1);
    result = tmp1;
    tmp4: while (true) {
      scrut = Runtime.stackResume !== null;
      if (scrut === true) {
        saved = Runtime.stackResume;
        Runtime.stackResume = null;
        Runtime.stackOffset = Runtime.stackDepth;
        tmp2 = runtime.safeCall(saved());
        result = tmp2;
        tmp3 = runtime.Unit;
        continue tmp4;
      } else {
        tmp3 = runtime.Unit;
      }
      break;
    }
    Runtime.stackLimit = 0;
    Runtime.stackDepth = 0;
    Runtime.stackOffset = 0;
    Runtime.stackHandler = null;
    return result
  }
  static toString() { return "Runtime"; }
});
let Runtime = Runtime1; export default Runtime;
"""))
