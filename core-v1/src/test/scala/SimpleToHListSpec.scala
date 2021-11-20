package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleToHListSpec extends FunSuite {

  import dsl.simple._

  test("Expr.ConcatToHList works when chained with Expr.WrapAs") {
    val expected = Person("Alice", 40)
    val xhl = expected.name.const :: expected.age.const
    val xp: Any ~:> Person = xhl.toHList.as[Person]
    val observed = xp.run()
    assertEquals(observed, expected)
  }
}

final case class Person(
  name: String,
  age: Int,
)
