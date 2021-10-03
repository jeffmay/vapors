package com.rallyhealth.vapors.v1

import munit.FunSuite

class StandardComparisonSpec extends FunSuite {

  import dsl.standard._

//  test("2 < 1 = false (expr)") {
//    val result = (2.const < 1.const).run()
//    assert(!result.state.output)
//  }

  test("2 < 1 = false (literal)") {
    val result = (2.const < 1).run()
    assert(!result.state.output)
  }

//  test("1 < 1 = false (expr)") {
//    val result = (1.const < 1.const).run()
//    assert(!result.state.output)
//  }

  test("1 < 1 = false (literal)") {
    val result = (1.const < 1).run()
    assert(!result.state.output)
  }

//  test("1 < 2 = true (expr)") {
//    val result = (1.const < 2.const).run()
//    assert(result.state.output)
//  }

  test("1 < 2 = true (literal)") {
    val result = (1.const < 2).run()
    assert(result.state.output)
  }

//  test("2 <= 1 = false (expr)") {
//    val result = (2.const <= 1.const).run()
//    assert(!result.state.output)
//  }
//
//  test("2 <= 1 = false (literal)") {
//    val result = (2.const <= 1).run()
//    assert(!result.state.output)
//  }
//
//  test("1 <= 1 = true (expr)") {
//    val result = (1.const <= 1.const).run()
//    assert(result.state.output)
//  }
//
//  test("1 <= 1 = true (literal)") {
//    val result = (1.const <= 1).run()
//    assert(result.state.output)
//  }
//
//  test("1 <= 2 = true (expr)") {
//    val result = (1.const <= 2.const).run()
//    assert(result.state.output)
//  }
//
//  test("1 <= 2 = true (literal)") {
//    val result = (1.const <= 2).run()
//    assert(result.state.output)
//  }
//
//  test("2 > 1 = true (expr)") {
//    val result = (2.const > 1.const).run()
//    assert(result.state.output)
//  }
//
//  test("2 > 1 = true (literal)") {
//    val result = (2.const > 1).run()
//    assert(result.state.output)
//  }
//
//  test("1 > 1 = false (expr)") {
//    val result = (1.const > 1.const).run()
//    assert(!result.state.output)
//  }
//
//  test("1 > 1 = false (literal)") {
//    val result = (1.const > 1).run()
//    assert(!result.state.output)
//  }
//
//  test("1 > 2 = false (expr)") {
//    val result = (1.const > 2.const).run()
//    assert(!result.state.output)
//  }
//
//  test("1 > 2 = false (literal)") {
//    val result = (1.const > 2).run()
//    assert(!result.state.output)
//  }
//
//  test("2 >= 1 = true (expr)") {
//    val result = (2.const >= 1.const).run()
//    assert(result.state.output)
//  }
//
//  test("2 >= 1 = true (literal)") {
//    val result = (2.const >= 1).run()
//    assert(result.state.output)
//  }
//
//  test("1 >= 1 = true (expr)") {
//    val result = (1.const >= 1.const).run()
//    assert(result.state.output)
//  }
//
//  test("1 >= 1 = true (literal)") {
//    val result = (1.const >= 1).run()
//    assert(result.state.output)
//  }
//
//  test("1 >= 2 = false (expr)") {
//    val result = (1.const >= 2.const).run()
//    assert(!result.state.output)
//  }
//
//  test("1 >= 2 = false (literal)") {
//    val result = (1.const >= 2).run()
//    assert(!result.state.output)
//  }

}
