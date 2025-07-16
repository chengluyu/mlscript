package hkmc2
package semantics
package importer

import syntax.LetBind, semantics.Elaborator.State
import mlscript.utils.*, shorthands.*

class DummyImporter(using State) extends Importer:
  def importPath(path: Str): Import =
    Import(TermSymbol(LetBind, N, new syntax.Tree.Ident(path)), path)
