package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleGetOrElseSpec extends FunSuite {

  import dsl.caching.immutable._

  test("Some().const.getOrElse returns the wrapped value") {
    val input = 1
    val expr = Some(input).const.getOrElse(2.const)
    assertEquals(expr.run(), input)
  }

  test("some().getOrElse returns the wrapped value") {
    val input = 1
    val expr = some(input.const).getOrElse(2.const)
    assertEquals(expr.run(), input)
  }

  test("none.getOrElse returns the default value") {
    val input = 2
    val expr = none[Int].getOrElse(input.const)
    assertEquals(expr.run(), input)
  }
}
