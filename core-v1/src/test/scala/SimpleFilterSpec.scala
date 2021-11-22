package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleFilterSpec extends FunSuite {

  import dsl.simple._

  test("Seq[Int].filter") {
    val expr = Seq(1, 2, 3, 4).const.filter(_ < 3.const)
    val res = expr.run()
    assertEquals(res, Seq(1, 2))
  }

  test("Seq[Nothing].filter doesn't compile") {
    compileErrors {
      "Seq().const.filter(_ < 3.const)"
    }
  }
}
