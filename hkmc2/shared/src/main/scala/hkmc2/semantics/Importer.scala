package hkmc2
package semantics


import mlscript.utils.*, shorthands.*


abstract class Importer:
  def importPath(path: Str): Import
