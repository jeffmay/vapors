package com.rallyhealth.vapors.v1

import algebra.ZipToHList
import data.{FactTable, Justified}
import engine.SimpleEngine

import cats.data.NonEmptyList
import munit.FunSuite
import shapeless.HNil

class SimpleJustifiedZipToHListSpec extends FunSuite {

  import dsl.simple.justified._

  test("justified hcons") {
    val x = 1.const :: "a".const
    val compute = x.zipToHListWith(ZipToHList.proxy(SimpleEngine[OP](FactTable.empty)))
    val res = compute(())
    val expected = Justified.byInference(
      "map",
      1 :: "a" :: HNil,
      NonEmptyList.of(
        Justified.byInference(
          "product",
          (1, "a" :: HNil),
          NonEmptyList.of(
            Justified.byConst(1),
            Justified.byConst("a" :: HNil),
          ),
        ),
      ),
    )
    assertEquals(res, expected)
  }
}
