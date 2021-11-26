package com.rallyhealth.vapors.v1

import CustomAssertions.assertMatchingOutcomes

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

import java.time.{Duration, Instant, LocalDate, Period}

class SimpleAdditionSpec extends munit.FunSuite {

  import dsl.simple._

  test("Int + Int (behavior)") {
    forAll { (l: Int, r: Int) =>
      val expr = l.const + r.const
      val expected = l + r
      val observed = expr.run()
      assertEquals(observed, expected)
    }
  }

  test("Int + Int + Int (syntax)") {
    val expr = 1.const + 2.const + 3.const
    val observed = expr.run()
    assertEquals(observed, 6)
  }

  test("Long + Int (behavior)") {
    forAll { (l: Long, r: Int) =>
      val expr = l.const + r.const
      val expected = l + r
      val observed = expr.run()
      assertEquals(observed, expected)
    }
  }

  test("Int + Long (syntax)") {
    val expr = 1.const + 2L.const
    val observed = expr.run()
    assertEquals(observed, 3L)
  }

  test("Int + Int + Long (syntax)") {
    val expr = 1.const + 2.const + 3L.const
    val observed = expr.run()
    assertEquals(observed, 6L)
  }

  test("Double + Int (behavior)") {
    forAll { (l: Double, r: Int) =>
      val expr = l.const + r.const
      val expected = l + r
      val observed = expr.run()
      assertEquals(observed, expected)
    }
  }

  test("Double (min) + Int (max)") {
    val expr = Double.MinValue.const + Int.MaxValue.const
    val observed = expr.run()
    val expected = Double.MinValue + Int.MaxValue
    assume(expected < 0, "should be negative")
    assertEquals(observed, expected)
  }

  test("Int (max) + Double (min)") {
    val expr = Int.MaxValue.const + Double.MinValue.const
    val observed = expr.run()
    val expected = Int.MaxValue + Double.MinValue
    assume(expected < 0, "should be negative")
    assertEquals(observed, expected)
  }

  test("Float + Int (behavior)") {
    forAll { (l: Float, r: Int) =>
      val expr = l.const + r.const
      val expected = l + r
      val observed = expr.run()
      assertEquals(observed, expected)
    }
  }

  test("Float (max) + Int (min)") {
    val expr = Float.MaxValue.const + Int.MinValue.const
    val observed = expr.run()
    val expected = Float.MaxValue + Int.MinValue
    assert(expected > 0, "should be positive")
    assertEquals(observed, expected)
  }

  test("Int (max) + Float (min)") {
    val expr = Int.MaxValue.const + Float.MinValue.const
    val observed = expr.run()
    val expected = Int.MaxValue + Float.MinValue
    assert(expected < 0, "should be negative")
    assertEquals(observed, expected)
  }

  test("Double + Float (behavior)") {
    forAll { (l: Double, r: Float) =>
      val expr = l.const + r.const
      val expected = l + r
      val observed = expr.run()
      assertEquals(observed, expected)
    }
  }

  test("Double (max) + Float (min)") {
    val expr = Double.MaxValue.const + Float.MinValue.const
    val observed = expr.run()
    val expected = Double.MaxValue + Float.MinValue
    assert(expected > 0, "should be positive")
    assertEquals(observed, expected)
  }

  test("Float (min) + Double (max)") {
    val expr = Float.MaxValue.const + Double.MinValue.const
    val observed = expr.run()
    val expected = Float.MaxValue + Double.MinValue
    assert(expected < 0, "should be negative")
    assertEquals(observed, expected)
  }

  test("Double + Long (behavior)") {
    forAll { (l: Double, r: Long) =>
      val expr = l.const + r.const
      val expected = l + r
      val observed = expr.run()
      assertEquals(observed, expected)
    }
  }

  test("Double + Double (behavior)") {
    forAll { (l: Double, r: Double) =>
      val expr = l.const + r.const
      val expected = l + r
      val observed = expr.run()
      assertEquals(observed, expected)
    }
  }

  test("Instant + Duration (behavior)") {
    forAll { (l: Instant, r: Duration) =>
      assertMatchingOutcomes {
        (l.const + r.const).run()
      } {
        l.plus(r)
      }
    }
  }

  test("Instant + Duration + Duration (syntax)") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val expr = now.const + duration.const + duration.const
    val observed = expr.run()
    val expected = now.plus(duration).plus(duration)
    assertEquals(observed, expected)
  }

  // to be consistent with subtraction, we want the left-hand side to always be the temporal value
  test("Duration + Instant (compiler error message)") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val error = compileErrors {
      "duration.const + now.const"
    }
    assert(error contains "try swapping the order of arguments to java.time.Instant + java.time.Duration")
    assert(error contains "you can define an implicit Add[java.time.Duration, java.time.Instant]")
  }

  test("LocalDate + Period (behavior)") {
    forAll { (l: LocalDate, r: Period) =>
      assertMatchingOutcomes {
        (l.const + r.const).run()
      } {
        l.plus(r)
      }
    }
  }

  test("LocalDate + Period + Period (syntax)") {
    val today = LocalDate.now()
    val period = Period.ofDays(5)
    val expr = today.const + period.const + period.const
    val observed = expr.run()
    val expected = today.plus(period).plus(period)
    assertEquals(observed, expected)
  }

  // to be consistent with subtraction, we want the left-hand side to always be the temporal value
  test("Period + LocalDate (compiler error message)") {
    val today = LocalDate.now()
    val period = Period.ofDays(5)
    val error = compileErrors {
      "period.const + today.const"
    }
    assert(error contains "try swapping the order of arguments to java.time.LocalDate + java.time.Period")
    assert(error contains "you can define an implicit Add[java.time.Period, java.time.LocalDate]")
  }
}
