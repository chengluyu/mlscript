package hkmc2
package semantics
package importer

import scala.collection.mutable
import scala.annotation.tailrec

import mlscript.utils.*, shorthands.*
import hkmc2.Message.MessageContext
import utils.TraceLogger

import Elaborator.*
import hkmc2.syntax.LetBind

abstract class Importer:
  def importPath(path: Str): Import
