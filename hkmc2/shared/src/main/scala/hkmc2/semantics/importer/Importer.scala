package hkmc2
package semantics
package importer

import scala.collection.mutable
import scala.annotation.tailrec

import mlscript.utils.*, shorthands.*
import hkmc2.Message.MessageContext
import utils.TraceLogger

import Elaborator.*
import syntax.{Keyword, LetBind}, syntax.Tree.StrLit

abstract class Importer:
  /** Import an MLscript module. */
  def importPath(path: Str): Import
  
  /** Import the content of a file. In the web demo, we need to import the
   *  contents of other .mls files. Manually copying and pasting is too
   *  inconvenient. So, we allow the use of `import "path/to/file.mls"` in
   *  some tests to directly embed the file content as a string in the source
   *  code. Do not implement this method if this feature is not needed. */
  def importContent(kw: Keyword, kwLoc: Opt[Loc], path: StrLit): Opt[Term] = N
