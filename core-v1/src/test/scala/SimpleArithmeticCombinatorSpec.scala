package com.rallyhealth.vapors.v1

import cats.data.NonEmptySeq
import munit.FunSuite

class SimpleArithmeticCombinatorSpec extends FunSuite {

  import dsl.uncached._

  test("min(2, 3, 1) == 1") {
    val expr = min(2.const, 3.const, 1.const)
    val expected = Seq(2, 3, 1).min
    val observed = expr.run()
    assertEquals(observed, expected)
  }

  test("max(2, 3, 1) == 3") {
    val expr = max(2.const, 3.const, 1.const)
    val expected = Seq(2, 3, 1).max
    val observed = expr.run()
    assertEquals(observed, expected)
  }

  test("sum(1, 2, 3) == 6") {
    val expr = sum(1.const, 2.const, 3.const)
    val expected = Seq(1, 2, 3).sum
    val observed = expr.run()
    assertEquals(observed, expected)
  }

  test("NonEmptySeq.of(2, 3, 1).min == 1") {
    val expr = NonEmptySeq.of(2, 3, 1).const.min
    val expected = Seq(2, 3, 1).min
    val observed = expr.run()
    assertEquals(observed, expected)
  }

  test("NonEmptySeq.of(2, 3, 1).max == 3") {
    val expr = NonEmptySeq.of(2, 3, 1).const.max
    val expected = Seq(2, 3, 1).max
    val observed = expr.run()
    assertEquals(observed, expected)
  }

  test("NonEmptySeq.of(1, 2, 3).sum == 6") {
    val expr = NonEmptySeq.of(2, 3, 1).const.sum
    val expected = Seq(1, 2, 3).sum
    val observed = expr.run()
    assertEquals(observed, expected)
  }
}
