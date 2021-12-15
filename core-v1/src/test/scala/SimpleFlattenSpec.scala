package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleFlattenSpec extends FunSuite {

  import dsl.simple._

  test("flatten(List[List[Char]])") {
    val values = List(1, 2, 3).map(i => List.fill(i)('A' + i))
    val expr = flatten(values.const)
    val expected = values.flatten
    val observed = expr.run()
    assertEquals(observed, expected)
  }
}
