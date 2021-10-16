package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleComparisonSpec extends FunSuite {

  import dsl.simple._

  test("2 < 1 = false (expr)") {
    val result = (2.const < 1.const).run()
    assert(!result)
  }

  test("2 < 1 = false (literal)") {
    val result = (2.const < 1).run()
    assert(!result)
  }

  test("1 < 1 = false (expr)") {
    val result = (1.const < 1.const).run()
    assert(!result)
  }

  test("1 < 1 = false (literal)") {
    val result = (1.const < 1).run()
    assert(!result)
  }

  test("1 < 2 = true (expr)") {
    val result = (1.const < 2.const).run()
    assert(result)
  }

  test("1 < 2 = true (literal)") {
    val result = (1.const < 2).run()
    assert(result)
  }

  test("2 <= 1 = false (expr)") {
    val result = (2.const <= 1.const).run()
    assert(!result)
  }

  test("2 <= 1 = false (literal)") {
    val result = (2.const <= 1).run()
    assert(!result)
  }

  test("1 <= 1 = true (expr)") {
    val result = (1.const <= 1.const).run()
    assert(result)
  }

  test("1 <= 1 = true (literal)") {
    val result = (1.const <= 1).run()
    assert(result)
  }

  test("1 <= 2 = true (expr)") {
    val result = (1.const <= 2.const).run()
    assert(result)
  }

  test("1 <= 2 = true (literal)") {
    val result = (1.const <= 2).run()
    assert(result)
  }

  test("2 > 1 = true (expr)") {
    val result = (2.const > 1.const).run()
    assert(result)
  }

  test("2 > 1 = true (literal)") {
    val result = (2.const > 1).run()
    assert(result)
  }

  test("1 > 1 = false (expr)") {
    val result = (1.const > 1.const).run()
    assert(!result)
  }

  test("1 > 1 = false (literal)") {
    val result = (1.const > 1).run()
    assert(!result)
  }

  test("1 > 2 = false (expr)") {
    val result = (1.const > 2.const).run()
    assert(!result)
  }

  test("1 > 2 = false (literal)") {
    val result = (1.const > 2).run()
    assert(!result)
  }

  test("2 >= 1 = true (expr)") {
    val result = (2.const >= 1.const).run()
    assert(result)
  }

  test("2 >= 1 = true (literal)") {
    val result = (2.const >= 1).run()
    assert(result)
  }

  test("1 >= 1 = true (expr)") {
    val result = (1.const >= 1.const).run()
    assert(result)
  }

  test("1 >= 1 = true (literal)") {
    val result = (1.const >= 1).run()
    assert(result)
  }

  test("1 >= 2 = false (expr)") {
    val result = (1.const >= 2.const).run()
    assert(!result)
  }

  test("1 >= 2 = false (literal)") {
    val result = (1.const >= 2).run()
    assert(!result)
  }

}
