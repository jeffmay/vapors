package com.rallyhealth.vapors.v1

import data.Justified

import cats.data.NonEmptySeq
import lens.DataPath
import munit.FunSuite

class SimpleJustifiedContainsAnySpec extends FunSuite {

  import dsl.simple.justified._

  private val valid = Set(2, 3)
  private val justifiedValidValues = Justified.byInference(
    "validValues",
    valid,
    NonEmptySeq.fromSeqUnsafe(Justified.elements(Justified.byConst(valid)).toSeq),
  )

  test("[1, 2].containsAny([2, 3]) == true") {
    val validInput = Seq(1, 2)
    val expr = validInput.const.containsAny(valid.const)
    val observed = expr.run()
    val expected = Justified.ByInference(
      "contains",
      true,
      NonEmptySeq.of(
        justifiedValidValues,
        Justified.byInference(
          "foundElements",
          Seq(2),
          NonEmptySeq.of(Justified.bySelection(2, DataPath.empty.atIndex(1), Justified.byConst(validInput))),
        ),
      ),
    )
    assertEquals(observed, expected)
  }

  test("[1].containsAny([2, 3]) == true") {
    val invalidInput = Seq(1)
    val expr = invalidInput.const.containsAny(valid.const)
    val observed = expr.run()
    val expected = Justified.ByInference(
      "contains",
      false,
      NonEmptySeq.of(
        justifiedValidValues,
        Justified.ByConst(List()),
      ),
    )
    assertEquals(observed, expected)
  }

  test("1 in Set(2, 3) == false") {
    val inputValue = 1
    val expr = inputValue.const in valid.const
    val observed = expr.run()
    val expected = Justified.ByInference(
      "contains",
      false,
      NonEmptySeq.of(
        justifiedValidValues,
        Justified.ByConst(List()),
      ),
    )
    assertEquals(observed, expected)
  }

  test("2 in Set(2, 3) == true") {
    val inputValue = 2
    val expr = inputValue.const in valid.const
    val observed = expr.run()
    val expected = Justified.ByInference(
      "contains",
      true,
      NonEmptySeq.of(
        justifiedValidValues,
        Justified.byInference(
          "foundElements",
          Seq(inputValue),
          NonEmptySeq.of(Justified.byConst(inputValue)),
        ),
      ),
    )
    assertEquals(observed, expected)
  }

  test("[1, 2].exists(_ in Set(2, 3)) == true") {
    val validInput = Seq(1, 2)
    val expr = validInput.const.exists(_ in valid.const)
    val observed = expr.run()
    val expected = Justified.byInference(
      "exists",
      true,
      NonEmptySeq.of(
        Justified.ByInference(
          "contains",
          true,
          NonEmptySeq.of(
            justifiedValidValues,
            Justified.byInference(
              "foundElements",
              Seq(2),
              NonEmptySeq.of(Justified.bySelection(2, DataPath.empty.atIndex(1), Justified.byConst(validInput))),
            ),
          ),
        ),
      ),
    )
    assertEquals(observed, expected)
  }

  test("[1].exists(_ in Set(2, 3)) == true") {
    val invalidInput = Seq(1)
    val expr = invalidInput.const.exists(_ in valid.const)
    val observed = expr.run()
    val expected = Justified.byInference(
      "exists",
      false,
      NonEmptySeq.of(
        Justified.ByInference(
          "contains",
          false,
          NonEmptySeq.of(
            justifiedValidValues,
            Justified.byConst(Seq()),
          ),
        ),
      ),
    )
    assertEquals(observed, expected)
  }
}
