//package com.rallyhealth.vapors.v1
//
//import CustomAssertions.assertMatchingOutcomes
//
//import munit.FunSuite
//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
//
//import java.time.{Duration, Instant, LocalDate, Period}
//
//class SimpleSubtractionSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  test("Int - Int (behavior)") {
//    forAll { (l: Int, r: Int) =>
//      val expr = l.const - r.const
//      val expected = l - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Int - Int - Int (syntax)") {
//    val expr = 2.const - 1.const - 1.const
//    val observed = expr.run()
//    assertEquals(observed, 0)
//  }
//
//  test("Long - Int (behavior)") {
//    forAll { (l: Long, r: Int) =>
//      val expr = l.const - r.const
//      val expected = l - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Long (max) - Int (max)") {
//    val expr = Long.MaxValue.const - Int.MaxValue.const
//    val observed = expr.run()
//    val expected = Long.MaxValue - Int.MaxValue
//    assert(expected > 0, "should be positive")
//    assertEquals(observed, expected)
//  }
//
//  test("Int - Long (behavior)") {
//    forAll { (l: Int, r: Long) =>
//      val expr = l.const - r.const
//      val expected = l - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Int (max) - Long (max)") {
//    val expr = Int.MaxValue.const - Long.MaxValue.const
//    val observed = expr.run()
//    val expected = Int.MaxValue - Long.MaxValue
//    assert(expected < 0, "should be negative")
//    assertEquals(observed, expected)
//  }
//
//  test("Double - Int (behavior)") {
//    forAll { (l: Double, r: Int) =>
//      val expr = l.const - r.const
//      val expected = l - r.toDouble
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Double (max) - Int (max)") {
//    val expr = Double.MaxValue.const - Int.MaxValue.const
//    val observed = expr.run()
//    val expected = Double.MaxValue - Int.MaxValue.toDouble
//    assert(expected > 0, "should be positive")
//    assertEquals(observed, expected)
//  }
//
//  test("Int - Double (behavior)") {
//    forAll { (l: Int, r: Double) =>
//      val expr = l.const - r.const
//      val expected = l.toDouble - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Int (max) - Double (max)") {
//    val expr = Int.MaxValue.const - Double.MaxValue.const
//    val observed = expr.run()
//    val expected = Int.MaxValue.toDouble - Double.MaxValue
//    assert(expected < 0, "should be negative")
//    assertEquals(observed, expected)
//  }
//
//  test("Float - Int (behavior)") {
//    forAll { (l: Float, r: Int) =>
//      val expr = l.const - r.const
//      val expected = l - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Float (max) - Int (max)") {
//    val expr = Float.MaxValue.const - Int.MaxValue.const
//    val observed = expr.run()
//    val expected = Float.MaxValue - Int.MaxValue.toFloat
//    assert(expected > 0, "should be positive")
//    assertEquals(observed, expected)
//  }
//
//  test("Int - Float (behavior)") {
//    forAll { (l: Int, r: Float) =>
//      val expr = l.const - r.const
//      val expected = l.toFloat - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Int (max) - Float (max)") {
//    val expr = Int.MaxValue.const - Float.MaxValue.const
//    val observed = expr.run()
//    val expected = Int.MaxValue.toFloat - Float.MaxValue
//    assert(expected < 0, "should be negative")
//    assertEquals(observed, expected)
//  }
//
//  test("Double - Float (behavior)") {
//    forAll { (l: Double, r: Float) =>
//      val expr = l.const - r.const
//      val expected = l - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Double (max) - Float (max)") {
//    val expr = Double.MaxValue.const - Float.MaxValue.const
//    val observed = expr.run()
//    val expected = Double.MaxValue - Float.MaxValue
//    assert(expected > 0, "should be positive")
//    assertEquals(observed, expected)
//  }
//
//  test("Double - Float (behavior)") {
//    forAll { (l: Double, r: Float) =>
//      val expr = l.const - r.const
//      val expected = l - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Float (max) - Double (max)") {
//    val expr = Float.MaxValue.const - Double.MaxValue.const
//    val observed = expr.run()
//    val expected = Float.MaxValue - Double.MaxValue
//    assert(expected < 0, "should be negative")
//    assertEquals(observed, expected)
//  }
//
//  test("Double - Long (behavior)") {
//    forAll { (l: Double, r: Long) =>
//      val expr = l.const - r.const
//      val expected = l - r.toDouble
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Long - Double (behavior)") {
//    forAll { (l: Long, r: Double) =>
//      val expr = l.const - r.const
//      val expected = l.toDouble - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Double - Double (behavior)") {
//    forAll { (l: Double, r: Double) =>
//      val expr = l.const - r.const
//      val expected = l - r
//      val observed = expr.run()
//      assertEquals(observed, expected)
//    }
//  }
//
//  test("Instant - Duration (behavior)") {
//    forAll { (l: Instant, r: Duration) =>
//      assertMatchingOutcomes {
//        (l.const + r.const).run()
//      } {
//        l.plus(r)
//      }
//    }
//  }
//
//  test("Instant - Duration - Duration (syntax)") {
//    val now = Instant.now()
//    val duration = Duration.ofMinutes(5)
//    val expr = now.const - duration.const - duration.const
//    val observed = expr.run()
//    val expected = now.minus(duration).minus(duration)
//    assertEquals(observed, expected)
//  }
//
//  // it is a little unclear what subtracting today from 5 minutes means, and it should be easy to swap
//  // the arguments to make it easier to read the expressions
//  test("Duration - Instant (compiler error message)") {
//    val now = Instant.now()
//    val duration = Duration.ofMinutes(5)
//    val error = compileErrors {
//      "duration.const - now.const"
//    }
//    assert(error contains "try swapping the order of arguments to java.time.Instant - java.time.Duration")
//    assert(error contains "you can define an implicit Subtract[java.time.Duration, java.time.Instant]")
//  }
//
//  test("LocalDate - Period (behavior)") {
//    forAll { (l: LocalDate, r: Period) =>
//      assertMatchingOutcomes {
//        (l.const + r.const).run()
//      } {
//        l.plus(r)
//      }
//    }
//  }
//
//  test("LocalDate - Period - Period (syntax)") {
//    val today = LocalDate.now()
//    val period = Period.ofDays(5)
//    val expr = today.const - period.const - period.const
//    val observed = expr.run()
//    val expected = today.minus(period).minus(period)
//    assertEquals(observed, expected)
//  }
//
//  // it is a little unclear what subtracting today from 5 days means, and it should be easy to swap
//  // the arguments to make it easier to read the expressions
//  test("Period - LocalDate (compiler error message)") {
//    val today = LocalDate.now()
//    val period = Period.ofDays(5)
//    val error = compileErrors {
//      "period.const - today.const"
//    }
//    assert(error contains "try swapping the order of arguments to java.time.LocalDate - java.time.Period")
//    assert(error contains "you can define an implicit Subtract[java.time.Period, java.time.LocalDate]")
//  }
//}
