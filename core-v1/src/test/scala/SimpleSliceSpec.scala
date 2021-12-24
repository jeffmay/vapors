package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleSliceSpec extends FunSuite {

  import dsl.simple._

  test("[1..5].slice(1 <-> 3)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(1 <-> 3)
    val observed = expr.run()
    val expected = input.slice(1, 3)
    assertEquals(observed, expected)
  }

  test("[1..5].slice(-3 <-> -1)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(-3 <-> -1)
    val observed = expr.run()
    val expected = input.slice(2, 4)
    assertEquals(observed, expected)
  }

  test("[1..5].slice(1 <-> -1)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(1 <-> -1)
    val observed = expr.run()
    val expected = input.slice(1, 4)
    assertEquals(observed, expected)
  }

  test("[1..5].slice(2 <-> End)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(2 <-> End)
    val observed = expr.run()
    val expected = input.slice(2, 5)
    assertEquals(observed, expected)
  }

  test("[1..5].slice(-2 <-> End)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(-2 <-> End)
    val observed = expr.run()
    val expected = input.slice(3, 5)
    assertEquals(observed, expected)
  }
}
