package com.rallyhealth.vapors.v1

import data.Justified

import cats.data.NonEmptySeq

class SimpleJustifiedMultiplicationSpec extends munit.FunSuite {

  import dsl.simple.justified._

  test("Justified[Int] * Justified[Int]") {
    val expr = 2.const * 2.const
    val observed = expr.run()
    val expected = Justified.byInference("multiply", 4, NonEmptySeq.of(Justified.byConst(2), Justified.byConst(2)))
    assertEquals(observed, expected)
  }

  test("Justified[Int] * Justified[Int] * Justified[Int]") {
    val expr = 2.const * 2.const * 2.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "multiply",
      8,
      NonEmptySeq.of(
        Justified.byInference("multiply", 4, NonEmptySeq.of(Justified.byConst(2), Justified.byConst(2))),
        Justified.byConst(2),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified[Long] + Justified[Int]") {
    val expr = 2L.const * 2.const
    val observed = expr.run()
    val expected = Justified.byInference("multiply", 4L, NonEmptySeq.of(Justified.byConst(2L), Justified.byConst(2)))
    assertEquals(observed, expected)
  }

  test("Justified[Int] + Justified[Long]") {
    val expr = 2.const * 2L.const
    val observed = expr.run()
    val expected = Justified.byInference("multiply", 4L, NonEmptySeq.of(Justified.byConst(2), Justified.byConst(2L)))
    assertEquals(observed, expected)
  }

  test("Justified[Int] * Justified[Int] * Justified[Long]") {
    val expr = 2.const * 2.const * 2L.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "multiply",
        8L,
        NonEmptySeq.of(
          Justified.byInference("multiply", 4, NonEmptySeq.of(Justified.byConst(2), Justified.byConst(2))),
          Justified.byConst(2L),
        ),
      ),
    )
  }

  test("Justified[Int] * Justified[Double]") {
    val expr = 2.const * 0.5d.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference("multiply", 1d, NonEmptySeq.of(Justified.byConst(2), Justified.byConst(0.5d))),
    )
  }

  test("Justified[Double] * Justified[Int]") {
    val expr = 0.5d.const * 2.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference("multiply", 1d, NonEmptySeq.of(Justified.byConst(0.5d), Justified.byConst(2))),
    )
  }

  test("Justified[Int] * Justified[Float]") {
    val expr = 2.const * 0.5f.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference("multiply", 1f, NonEmptySeq.of(Justified.byConst(2), Justified.byConst(0.5f))),
    )
  }

  test("Justified[Double] * Justified[Float]") {
    val expr = 0.5d.const * 2f.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference("multiply", 1d, NonEmptySeq.of(Justified.byConst(0.5d), Justified.byConst(2f))),
    )
  }

  test("Justified[Float] * Justified[Double]") {
    val expr = 2f.const * 0.5d.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference("multiply", 1d, NonEmptySeq.of(Justified.byConst(2f), Justified.byConst(0.5d))),
    )
  }

  test("Justified[Float] * Justified[Int]") {
    val expr = 0.5f.const * 2.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference("multiply", 1f, NonEmptySeq.of(Justified.byConst(0.5f), Justified.byConst(2))),
    )
  }

  test("Justified[String] * Justified[Int]") {
    val expr = "*".const * 4.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "multiply",
      "*" * 4,
      NonEmptySeq.of(Justified.byConst("*"), Justified.byConst(4)),
    )
    assertEquals(observed, expected)
  }

  test("Justified[String] * Justified[Int] * Justified[Int]") {
    val expr = "*".const * 2.const * 2.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "multiply",
      "*" * 4,
      NonEmptySeq.of(
        Justified.byInference(
          "multiply",
          "*" * 2,
          NonEmptySeq.of(Justified.byConst("*"), Justified.byConst(2)),
        ),
        Justified.byConst(2),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified[Int] * Justified[String] (compiler error message)") {
    val _ = compileErrors("""4 * "*"""")
    val error = compileErrors("""4.const * "*".const""")
    assert(
      error contains "try swapping the order of arguments to com.rallyhealth.vapors.v1.data.Justified[String] * com.rallyhealth.vapors.v1.data.Justified[Int]",
    )
    assert(
      error contains "you can define an implicit Multiply[com.rallyhealth.vapors.v1.data.Justified[Int], com.rallyhealth.vapors.v1.data.Justified[String]]",
    )
  }
}
