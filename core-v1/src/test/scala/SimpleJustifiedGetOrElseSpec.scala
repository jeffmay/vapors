package com.rallyhealth.vapors.v1

import data.Justified
import lens.DataPath

import munit.FunSuite

class SimpleJustifiedGetOrElseSpec extends FunSuite {

  import dsl.caching.immutable.justified._

  test("Some().const.getOrElse returns the wrapped value") {
    val input = 1
    val expr = Some(input).const.getOrElse(2.const)
    val expected = Justified.bySelection(1, DataPath.empty.atIndex(0), Justified.byConst(Some(input)))
    assertEquals(expr.run(), expected)
  }

  test("some().getOrElse returns the wrapped value") {
    val input = 1
    val expr = some(input.const).getOrElse(2.const)
    val expected = Justified.byConst(input)
    assertEquals(expr.run(), expected)
  }

  test("none.getOrElse returns the default value") {
    val input = 2
    val expr = none[Int].getOrElse(input.const)
    val expected = Justified.byConst(input)
    assertEquals(expr.run(), expected)
  }
}
