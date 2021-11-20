package com.rallyhealth.vapors.v1

import data.Justified

import munit.FunSuite
import shapeless.HNil

class SimpleJustifiedConcatToHListSpec extends FunSuite {

  import dsl.simple.justified._

  test("Expr.ConcatToHList returns individual justified values") {
    val expected = Justified.byConst("Alice") :: Justified.byConst(40) :: HNil
    val xhl = "Alice".const :: 40.const
    val xp = xhl.concatToHList
    val observed = xp.run()
    assertEquals(observed, expected)
  }
}
