package com.rallyhealth.vapors.v1

import data.Justified

import cats.data.NonEmptyList

class SimpleJustifiedArithmeticSpec extends munit.FunSuite {

  import java.time.{Duration, Instant}

  import dsl.simple.justified._

  test("Justified[Int] + Justified[Int]") {
    val expr = 1.const + 2.const
    val observed = expr.run()
    val expected = Justified.byInference("add", 3, NonEmptyList.of(Justified.byConst(1), Justified.byConst(2)))
    assertEquals(observed, expected)
  }

  test("Justified[Long] + Justified[Int]") {
    val expr = 1L.const.+(2.const)
    val observed = expr.run()
    val expected = Justified.byInference("add", 3L, NonEmptyList.of(Justified.byConst(1L), Justified.byConst(2)))
    assertEquals(observed, expected)
  }

  test("Justified[Int] + Justified[Long]") {
    val expr = 1.const + 2L.const
    val observed = expr.run()
    val expected = Justified.byInference("add", 3L, NonEmptyList.of(Justified.byConst(1), Justified.byConst(2L)))
    assertEquals(observed, expected)
  }

  test("Justified[Instant] + Justified[Duration]") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val expr = now.const + duration.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      now.plus(duration),
      NonEmptyList.of(Justified.byConst(now), Justified.byConst(duration)),
    )
    assertEquals(observed, expected)
  }

  test("Justified[Duration] + Justified[Instant]") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val expr = duration.const + now.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      now.plus(duration),
      NonEmptyList.of(Justified.byConst(duration), Justified.byConst(now)),
    )
    assertEquals(observed, expected)
  }

  // TODO: LocalDate + Period
}
