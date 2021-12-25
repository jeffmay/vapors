package com.rallyhealth.vapors.v1

import data.Justified

import munit.FunSuite

class SimpleJustifiedFilterSpec extends FunSuite {

  import dsl.simple.justified._

  test("Seq[Justified[Int]].filter") {
    val input = Seq(1, 2, 3, 4)
    val expr = input.const.filter(_ < 3.const)
    val expected = Justified.elements(Justified.byConst(input)).filter(_.value < 3)
    val res = expr.run()
    assertEquals(res, expected)
  }

  test("Seq[Justified[Nothing]].filter doesn't compile") {
    compileErrors {
      "Seq().const.filter(_ < 3.const)"
    }
  }
}
