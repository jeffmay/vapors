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

  test("Vector[Int].flatMap(Seq[Int])") {
    val values = Vector(1, 2, 3)
    val innerValues = Seq(2, 3, 4)
    val expr = values.const.flatMap { _ =>
      innerValues.const
    }
    val observed = expr.run()
    val expected = values.flatMap(_ => innerValues)
    assertEquals(observed, expected)
  }
}
