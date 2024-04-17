package hkmc2

package object syntax:
  /** Match something like `$$Lambda$188/0x00000070011679a0@ba54932` */
  val lambdaPattern = """\$Lambda\$\d+/0x[\da-fA-F]+@[\da-fA-F]+""".r
  extension (x: Any)
    def toStableString: String =
      lambdaPattern.replaceAllIn(x.toString, "Lambda")
