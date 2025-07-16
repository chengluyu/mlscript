package hkmc2

import utils.path.*

class PathTests extends org.scalatest.funsuite.AnyFunSuite:
  test("relativeTo - Unix"):
    val dev = AbsolutePath("Users", "john", "Developer")
    val home = AbsolutePath("Users", "john")
    assert(dev.relativeTo(home) == "./Developer")
