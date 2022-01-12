package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleIsEqualSpec extends FunSuite {

  import dsl.simple._

  test("1 === 1 is true") {
    val expr = 1.const === 1.const
    assert(expr.run())
  }

  test("1 === 2 is false") {
    val expr = 1.const === 2.const
    assert(!expr.run())
  }

  test("1 =!= 1 is false") {
    val expr = 1.const =!= 1.const
    assert(!expr.run())
  }

  test("1 =!= 2 is true") {
    val expr = 1.const =!= 2.const
    assert(expr.run())
  }
}
