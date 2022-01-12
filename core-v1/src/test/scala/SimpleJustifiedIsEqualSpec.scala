package com.rallyhealth.vapors.v1

import data.Justified

import cats.data.NonEmptySeq
import munit.FunSuite

class SimpleJustifiedIsEqualSpec extends FunSuite {

  import dsl.simple.justified._

  test("1 === 1 is true") {
    val expr = 1.const === 1.const
    val observed = expr.run()
    val expected = Justified.byInference("isEqual", true, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(1)))
    assertEquals(observed, expected)
  }

  test("1 === 2 is false") {
    val expr = 1.const === 2.const
    val observed = expr.run()
    val expected = Justified.byInference("isEqual", false, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(2)))
    assertEquals(observed, expected)
  }

  test("1 =!= 1 is false") {
    val expr = 1.const =!= 1.const
    val observed = expr.run()
    val expected = Justified.ByInference(
      "not",
      false,
      NonEmptySeq.of(
        Justified.byInference("isEqual", true, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(1))),
      ),
    )
    assertEquals(observed, expected)
  }

  test("1 =!= 2 is true") {
    val expr = 1.const =!= 2.const
    val observed = expr.run()
    val expected = Justified.ByInference(
      "not",
      true,
      NonEmptySeq.of(
        Justified.byInference("isEqual", false, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(2))),
      ),
    )
    assertEquals(observed, expected)
  }
}
