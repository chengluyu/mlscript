package hkmc2
package utils

import mlscript.utils.*, shorthands.*

object PathOps:
  def baseName(path: Str): Str =
    val i = path.lastIndexOf('/')
    if i < 0 then path else path.substring(i + 1)
  
  def parent(path: Str): Str =
    if path.isEmpty || path == "/" then "/"
    else
      val i = path.lastIndexOf('/')
      if i <= 0 then "/" else path.substring(0, i)
  
  def split(path: Str): (Str, Str) =
    val i = path.lastIndexOf('/')
    if i < 0 then (path, "") else (path.take(i), path.drop(i + 1))
  
  def relativeTo(path: String, base: String): String =
    print(s"relativeTo:\n  $path\n  $base\n")
    def go(p: String, b: String): String =
      print(s"step:\n  p = $p\n  b = $b\n")
      val (ph, pt) = split(p)
      val (bh, bt) = split(b)
      if ph == bh then go(pt, bt) else "../" * b.count(_ == '/') + p
    go(path, base)