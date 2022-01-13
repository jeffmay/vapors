package com.rallyhealth.vapors.v1

class SimpleMultiplicationSpec extends munit.FunSuite {

  import dsl.caching.immutable._

  test("Int * Int") {
    val expr = 2.const * 2.const
    val observed = expr.run()
    assertEquals(observed, 4)
  }

  test("Int * Int * Int") {
    val expr = 2.const * 2.const * 2.const
    val observed = expr.run()
    assertEquals(observed, 8)
  }

  test("Long * Int") {
    val expr = 2L.const * 2.const
    val observed = expr.run()
    assertEquals(observed, 4L)
  }

  test("Int * Long") {
    val expr = 2.const * 2L.const
    val observed = expr.run()
    assertEquals(observed, 4L)
  }

  test("Int * Int * Long") {
    val expr = 2.const * 2.const * 2L.const
    val observed = expr.run()
    assertEquals(observed, 8L)
  }

  test("Int * Double") {
    val expr = 2.const * 0.5d.const
    val observed = expr.run()
    assertEquals(observed, 1d)
  }

  test("Double * Int") {
    val expr = 0.5d.const * 2.const
    val observed = expr.run()
    assertEquals(observed, 1d)
  }

  test("Int * Float") {
    val expr = 2.const * 0.5f.const
    val observed = expr.run()
    assertEquals(observed, 1f)
  }

  test("Double * Float") {
    val expr = 0.5d.const * 2f.const
    val observed = expr.run()
    assertEquals(observed, 1d)
  }

  test("Float * Double") {
    val expr = 2f.const * 0.5d.const
    val observed = expr.run()
    assertEquals(observed, 1d)
  }

  test("Float * Int") {
    val expr = 0.5f.const * 2.const
    val observed = expr.run()
    assertEquals(observed, 1f)
  }

  test("String * Int") {
    val expr = "*".const * 4.const
    val observed = expr.run()
    val expected = "*" * 4
    assertEquals(observed, expected)
  }

  test("String * Int * Int") {
    val expr = "*".const * 2.const * 2.const
    val observed = expr.run()
    val expected = "*" * 2 * 2
    assertEquals(observed, expected)
  }

  // to be consistent with the definition of string multiples in Scala
  test("Int * String (compiler error message)") {
    val _ = compileErrors("""4 * "*"""")
    val error = compileErrors("""4.const * "*".const""")
    assert(error contains "try swapping the order of arguments to String * Int")
    assert(error contains "you can define an implicit Multiply[Int, String]")
  }
}
