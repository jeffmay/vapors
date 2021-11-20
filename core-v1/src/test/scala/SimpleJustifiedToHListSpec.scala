package com.rallyhealth.vapors.v1

import data.Justified
import cats.data.NonEmptySeq
import munit.FunSuite
import shapeless.HNil

class SimpleJustifiedToHListSpec extends FunSuite {

  import dsl.simple.justified._

  test("justified hcons") {
    val expr = (1.const :: "a".const).toHList
    val res = expr.run()
    val expected = Justified.byInference(
      "map",
      1 :: "a" :: HNil,
      NonEmptySeq.of(
        Justified.byInference(
          "product",
          (1, "a" :: HNil),
          NonEmptySeq.of(
            Justified.byConst(1),
            Justified.byConst("a" :: HNil),
          ),
        ),
      ),
    )
    assertEquals(res, expected)
  }
}
