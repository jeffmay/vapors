package com.rallyhealth.vapors.v1

import munit.FunSuite

class StandardLogicalSpec extends FunSuite {

  import dsl.standard._

  test("!false == true") {
    val expr = !false.const
    val result = expr.run()
    assert(result.state.output)
  }

  test("!true == false") {
    val expr = !true.const
    val result = expr.run()
    assert(!result.state.output)
  }

  test("not(false) == true") {
    val expr = not(false.const)
    val result = expr.run()
    assert(result.state.output)
  }

  test("not(true) == false") {
    val expr = not(true.const)
    val result = expr.run()
    assert(!result.state.output)
  }
}
