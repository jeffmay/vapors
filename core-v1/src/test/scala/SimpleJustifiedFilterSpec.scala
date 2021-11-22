package com.rallyhealth.vapors.v1

import com.rallyhealth.vapors.v1.data.Justified
import munit.FunSuite

class SimpleJustifiedFilterSpec extends FunSuite {

  import dsl.simple.justified._

  test("Seq[Justified[Int]].filter") {
    val expr = Seq(1, 2, 3, 4).const.filter(_ < 3.const)
    val res = expr.run()
    assertEquals(res, Seq(Justified.byConst(1), Justified.byConst(2)))
  }

  test("Seq[Justified[Nothing]].filter doesn't compile") {
    compileErrors {
      "Seq().const.filter(_ < 3.const)"
    }
  }
}
