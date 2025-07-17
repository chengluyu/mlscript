package hkmc2
package semantics
package importer

import scala.collection.mutable
import scala.annotation.tailrec

import mlscript.utils.*, shorthands.*
import hkmc2.Message.MessageContext
import utils.TraceLogger
import utils.path.*, conversion.*

import Elaborator.*
import syntax.{Keyword, LetBind}, syntax.Tree.StrLit

class FileImporter(val prelude: Ctx, val wd: os.Path)
    (using tl: TraceLogger)(using State, Raise) extends Importer:
  import tl.*
  
  // log(s"pwd: ${os.pwd}")
  // log(s"wd: ${wd}")
  
  def importPath(path: Str): Import =
    val file =
      if path.startsWith("/")
      then os.Path(path)
      else wd / os.RelPath(path)
    
    val nme = file.baseName
    val id = new syntax.Tree.Ident(nme) // TODO loc
    
    lazy val sym = TermSymbol(LetBind, N, id)
    
    if path.startsWith(".") || path.startsWith("/") then // leave alone imports like "fs"
      log(s"importing $file")
      
      val nme = file.baseName
      val id = new syntax.Tree.Ident(nme) // TODO loc
      
      file.ext match
      
      case "mjs" | "js" =>
        Import(sym, file.toString)
        
      case "mls" =>
        
        val block = os.read(file)
        val fph = new FastParseHelpers(block)
        val origin = Origin(file.toAbsolutePath, 0, fph)
        
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
          val importer = new FileImporter(prelude, file / os.up)
          val elab = Elaborator(tl, importer)
          elab.importFrom(resBlk)
          
          resBlk.definedSymbols.find(_._1 === nme) match
          case Some(nme -> sym) => sym
          case None => lastWords(s"File $file does not define a symbol named $nme")
        
        val jsFile = file / os.up / (file.baseName + ".mjs")
        Import(sym, jsFile.toString)
        
      case _ =>
        raise(ErrorReport(msg"Unsupported file extension: ${file.ext}" -> N :: Nil))
        Import(sym, file.toString)
      
    else
      Import(sym, path)
  
  override def importContent(kw: Keyword, kwLoc: Opt[Loc], path: StrLit): Opt[Term] =
    import java.nio.file.*
    var projectRoot = os.Path(Paths.get(".").toAbsolutePath())
    // The project root path is different in DiffTests and compilation tests.
    if !os.exists(projectRoot / "build.sbt") then
      projectRoot = projectRoot / os.up
    val filePath = projectRoot / os.RelPath(path.value)
    S(Term.Lit(StrLit(os.read(filePath))))
