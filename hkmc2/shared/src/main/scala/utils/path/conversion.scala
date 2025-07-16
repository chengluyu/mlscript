package hkmc2.utils.path

/** Do not import this object when in Scala.js. */
object conversion:
  extension (path: os.Path)
    def toAbsolutePath: AbsolutePath = AbsolutePath(path.segments.toSeq*)
