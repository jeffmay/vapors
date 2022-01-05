package com.rallyhealth.vapors.v1

import data.Justified

import cats.data.NonEmptySeq
import com.rallyhealth.vapors.v1.lens.DataPath
import munit.FunSuite

class SimpleJustifiedArithmeticCombinatorSpec extends FunSuite {

  import dsl.simple.justified._

  test("min(2, 3, 1) == 1") {
    val expr = min(2.const, 3.const, 1.const)
    val observed = expr.run()
    val expectedValue = Seq(2, 3, 1).min
    val expected = Justified.byConst(expectedValue)
    assertEquals(observed, expected)
  }

  test("max(2, 3, 1) == 3") {
    val expr = max(2.const, 3.const, 1.const)
    val observed = expr.run()
    val expectedValue = Seq(2, 3, 1).max
    val expected = Justified.byConst(expectedValue)
    assertEquals(observed, expected)
  }

  test("sum(1, 2, 3) == 6") {
    val expr = sum(1.const, 2.const, 3.const)
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      6,
      NonEmptySeq.of(
        Justified.byInference(
          "add",
          3,
          NonEmptySeq.of(
            Justified.ByInference("add", 1, NonEmptySeq.of(Justified.byConst(0), Justified.byConst(1))),
            Justified.byConst(2),
          ),
        ),
        Justified.byConst(3),
      ),
    )
    assertEquals(observed, expected)
  }

  test("NonEmptySeq.of(2, 3, 1).min == 1") {
    val input = NonEmptySeq.of(2, 3, 1)
    val expr = input.const.min
    val observed = expr.run()
    val expectedIndex = 2
    val expected = Justified.bySelection(
      input.getUnsafe(expectedIndex),
      DataPath.empty.atIndex(expectedIndex),
      Justified.byConst(input),
    )
    assertEquals(observed, expected)
  }

  test("NonEmptySeq.of(2, 3, 1).max == 3") {
    val input = NonEmptySeq.of(2, 3, 1)
    val expr = input.const.max
    val observed = expr.run()
    val expectedIndex = 1
    val expected = Justified.bySelection(
      input.getUnsafe(expectedIndex),
      DataPath.empty.atIndex(expectedIndex),
      Justified.byConst(input),
    )
    assertEquals(observed, expected)
  }

  test("NonEmptySeq.of(1, 2, 3).sum == 6") {
    val input = NonEmptySeq.of(2, 3, 1)
    val expr = input.const.sum
    val observed = expr.run()
    val expectedValue = Seq(2, 3, 1).sum
    val originalJustified = Justified.byConst(input)
    val expected = Justified.byInference(
      "add",
      6,
      NonEmptySeq.of(
        Justified.byInference(
          "add",
          5,
          NonEmptySeq.of(
            Justified.byInference(
              "add",
              2,
              NonEmptySeq.of(
                Justified.byConst(0),
                Justified.bySelection(2, DataPath.empty.atIndex(0), originalJustified),
              ),
            ),
            Justified.bySelection(3, DataPath.empty.atIndex(1), originalJustified),
          ),
        ),
        Justified.bySelection(1, DataPath.empty.atIndex(2), originalJustified),
      ),
    )
    assertEquals(observed, expected)
  }
}
