package com.rallyhealth.vapors.v1

import munit.FunSuite

class LogicalSpec extends FunSuite {

  import dsl.standard._

  test("not(false) is true") {
    val expr = !false.const
    val result = expr.run()
    assert(result.state.output)
  }

  test("not(false) is true") {
    val result = false.const.not.run()
    assert(result.state.output)
  }
}
