package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleFilterSpec extends FunSuite {

  import dsl.simple._

  test("filter a seq") {
    val expr = Seq(1, 2, 3, 4).const.filter(_ < 3)
    val res = expr.run()
    assertEquals(res, Seq(1, 2))
  }
}
