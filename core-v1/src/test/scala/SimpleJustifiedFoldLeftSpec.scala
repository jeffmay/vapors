package com.rallyhealth.vapors.v1

import data.Justified

import cats.data.NonEmptySeq
import lens.DataPath
import munit.FunSuite

class SimpleJustifiedFoldLeftSpec extends FunSuite {

  import dsl.uncached.justified._

  test(".foldLeft returns initial value when empty") {
    val expr = Seq.empty[Int].const.foldLeft(1.const)(_ + _)
    val observed = expr.run()
    assertEquals(observed, Justified.byConst(1))
  }

  test(".foldLeft returns summation of integers") {
    val input = Seq(1, 2, 3)
    val expr = input.const.foldLeft(1.const)(_ + _)
    val observed = expr.run()
    val inputWrapped = Justified.byConst(input)
    val expectedWrapped = Justified.byInference(
      "add",
      7,
      NonEmptySeq.of(
        Justified.byInference(
          "add",
          4,
          NonEmptySeq.of(
            Justified.byInference(
              "add",
              2,
              NonEmptySeq.of(
                Justified.byConst(1),
                Justified.bySelection(1, DataPath.empty.atIndex(0), inputWrapped),
              ),
            ),
            Justified.bySelection(2, DataPath.empty.atIndex(1), inputWrapped),
          ),
        ),
        Justified.bySelection(3, DataPath.empty.atIndex(2), inputWrapped),
      ),
    )
    assertEquals(observed, expectedWrapped)
  }
}
