package com.rallyhealth.vapors.v1

import data.Justified
import cats.data.NonEmptySeq

import java.time.{LocalDate, Period}

class SimpleJustifiedAdditionSpec extends munit.FunSuite {

  import java.time.{Duration, Instant}

  import dsl.uncached.justified._

  test("Justified[Int] + Justified[Int]") {
    val expr = 1.const + 2.const
    val observed = expr.run()
    val expected = Justified.byInference("add", 3, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(2)))
    assertEquals(observed, expected)
  }

  test("Justified[Int] + Justified[Int] + Justified[Int]") {
    val expr = 1.const + 2.const + 3.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      6,
      NonEmptySeq.of(
        Justified.byInference("add", 3, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(2))),
        Justified.byConst(3),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified[Long] + Justified[Int]") {
    val expr = 1L.const.+(2.const)
    val observed = expr.run()
    val expected = Justified.byInference("add", 3L, NonEmptySeq.of(Justified.byConst(1L), Justified.byConst(2)))
    assertEquals(observed, expected)
  }

  test("Justified[Int] + Justified[Long]") {
    val expr = 1.const + 2L.const
    val observed = expr.run()
    val expected = Justified.byInference("add", 3L, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(2L)))
    assertEquals(observed, expected)
  }

  test("Justified[Int] + Justified[Int] + Justified[Long]") {
    val expr = 1.const + 2.const + 3L.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      6L,
      NonEmptySeq.of(
        Justified.byInference("add", 3, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(2))),
        Justified.byConst(3L),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Double (min) + Int (max)") {
    val expr = Double.MinValue.const + Int.MaxValue.const
    val observed = expr.run()
    val expected = Double.MinValue + Int.MaxValue
    assume(expected < 0, "should be negative")
    assertEquals(
      observed,
      Justified.byInference(
        "add",
        expected,
        NonEmptySeq.of(Justified.byConst(Double.MinValue), Justified.byConst(Int.MaxValue)),
      ),
    )
  }

  test("Int (max) + Double (min)") {
    val expr = Int.MaxValue.const + Double.MinValue.const
    val observed = expr.run()
    val expected = Int.MaxValue + Double.MinValue
    assume(expected < 0, "should be negative")
    assertEquals(
      observed,
      Justified.byInference(
        "add",
        expected,
        NonEmptySeq.of(Justified.byConst(Int.MaxValue), Justified.byConst(Double.MinValue)),
      ),
    )
  }

  test("Float (max) + Int (min)") {
    val expr = Float.MaxValue.const + Int.MinValue.const
    val observed = expr.run()
    val expected = Float.MaxValue + Int.MinValue
    assert(expected > 0, "should be positive")
    assertEquals(
      observed,
      Justified.byInference(
        "add",
        expected,
        NonEmptySeq.of(Justified.byConst(Float.MaxValue), Justified.byConst(Int.MinValue)),
      ),
    )
  }

  test("Int (max) + Float (min)") {
    val expr = Int.MaxValue.const + Float.MinValue.const
    val observed = expr.run()
    val expected = Int.MaxValue + Float.MinValue
    assert(expected < 0, "should be negative")
    assertEquals(
      observed,
      Justified.byInference(
        "add",
        expected,
        NonEmptySeq.of(Justified.byConst(Int.MaxValue), Justified.byConst(Float.MinValue)),
      ),
    )
  }

  test("Double (max) + Float (min)") {
    val expr = Double.MaxValue.const + Float.MinValue.const
    val observed = expr.run()
    val expected = Double.MaxValue + Float.MinValue
    assert(expected > 0, "should be positive")
    assertEquals(
      observed,
      Justified.byInference(
        "add",
        expected,
        NonEmptySeq.of(Justified.byConst(Double.MaxValue), Justified.byConst(Float.MinValue)),
      ),
    )
  }

  test("Float (min) + Double (max)") {
    val expr = Float.MaxValue.const + Double.MinValue.const
    val observed = expr.run()
    val expected = Float.MaxValue + Double.MinValue
    assert(expected < 0, "should be negative")
    assertEquals(
      observed,
      Justified.byInference(
        "add",
        expected,
        NonEmptySeq.of(Justified.byConst(Float.MaxValue), Justified.byConst(Double.MinValue)),
      ),
    )
  }

  test("Justified[Instant] + Justified[Duration]") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val expr = now.const + duration.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      now.plus(duration),
      NonEmptySeq.of(Justified.byConst(now), Justified.byConst(duration)),
    )
    assertEquals(observed, expected)
  }

  test("Justified[Instant] + Justified[Duration] + Justified[Duration]") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val expr = now.const + duration.const + duration.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      now.plus(duration).plus(duration),
      NonEmptySeq.of(
        Justified.byInference(
          "add",
          now.plus(duration),
          NonEmptySeq.of(Justified.byConst(now), Justified.byConst(duration)),
        ),
        Justified.byConst(duration),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified[Duration] + Justified[Instant] (compiler error message)") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val error = compileErrors {
      "duration.const + now.const"
    }
    assert(
      error contains "try swapping the order of arguments to com.rallyhealth.vapors.v1.data.Justified[java.time.Instant] + com.rallyhealth.vapors.v1.data.Justified[java.time.Duration]",
    )
    assert(
      error contains "you can define an implicit Add[com.rallyhealth.vapors.v1.data.Justified[java.time.Duration], com.rallyhealth.vapors.v1.data.Justified[java.time.Instant]]",
    )
  }

  test("Justified[LocalDate] + Justified[Period]") {
    val today = LocalDate.now()
    val period = Period.ofDays(5)
    val expr = today.const + period.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      today.plus(period),
      NonEmptySeq.of(Justified.byConst(today), Justified.byConst(period)),
    )
    assertEquals(observed, expected)
  }

  test("Justified[LocalDate] + Justified[Period] + Justified[Period]") {
    val today = LocalDate.now()
    val period = Period.ofDays(5)
    val expr = today.const + period.const + period.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "add",
      today.plus(period).plus(period),
      NonEmptySeq.of(
        Justified
          .byInference("add", today.plus(period), NonEmptySeq.of(Justified.byConst(today), Justified.byConst(period))),
        Justified.byConst(period),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified[Period] + Justified[LocalDate] (compiler error message)") {
    val today = LocalDate.now()
    val period = Period.ofDays(5)
    val error = compileErrors {
      "period.const + today.const"
    }
    assert(
      error contains "try swapping the order of arguments to com.rallyhealth.vapors.v1.data.Justified[java.time.LocalDate] + com.rallyhealth.vapors.v1.data.Justified[java.time.Period]",
    )
    assert(
      error contains "you can define an implicit Add[com.rallyhealth.vapors.v1.data.Justified[java.time.Period], com.rallyhealth.vapors.v1.data.Justified[java.time.LocalDate]]",
    )
  }
}
