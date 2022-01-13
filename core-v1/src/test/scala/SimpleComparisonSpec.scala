package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleComparisonSpec extends FunSuite {

  import dsl.caching.immutable._

  test("2 < 1 = false") {
    val result = (2.const < 1.const).run()
    assert(!result)
  }

  test("1 < 1 = false") {
    val result = (1.const < 1.const).run()
    assert(!result)
  }

  test("1 < 2 = true") {
    val result = (1.const < 2.const).run()
    assert(result)
  }

  test("2 <= 1 = false") {
    val result = (2.const <= 1.const).run()
    assert(!result)
  }

  test("1 <= 1 = true") {
    val result = (1.const <= 1.const).run()
    assert(result)
  }

  test("1 <= 2 = true") {
    val result = (1.const <= 2.const).run()
    assert(result)
  }

  test("2 > 1 = true") {
    val result = (2.const > 1.const).run()
    assert(result)
  }

  test("1 > 1 = false") {
    val result = (1.const > 1.const).run()
    assert(!result)
  }

  test("1 > 2 = false") {
    val result = (1.const > 2.const).run()
    assert(!result)
  }

  test("2 >= 1 = true") {
    val result = (2.const >= 1.const).run()
    assert(result)
  }

  test("1 >= 1 = true") {
    val result = (1.const >= 1.const).run()
    assert(result)
  }

  test("1 >= 2 = false") {
    val result = (1.const >= 2.const).run()
    assert(!result)
  }

}
