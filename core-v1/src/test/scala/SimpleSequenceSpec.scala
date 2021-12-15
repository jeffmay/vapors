package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleSequenceSpec extends FunSuite {

  import dsl.simple._

  test("wrapAll List of mixed input expressions") {
    val expr = wrapAll(List(1.const, (ident[Int] + 1.const).toExpr, (ident[Int] * 2.const).toExpr)).map(_ * 2.const)
    val input = 2
    val expected = List(1, input + 1, input * 2).map(_ * 2)
    val observed = expr.runWith(input)
    assertEquals(observed, expected)
  }
}
