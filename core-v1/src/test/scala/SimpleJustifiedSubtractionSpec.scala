//package com.rallyhealth.vapors.v1
//
//import data.Justified
//import cats.data.NonEmptySeq
//import munit.FunSuite
//
//import java.time.{Duration, Instant, LocalDate, Period}
//
//class SimpleJustifiedSubtractionSpec extends FunSuite {
//
//  import dsl.uncached.justified._
//
//  // TODO: Should this return an Option[Int]? Maybe another method for this?
//  test("Divide by zero") {
//    val expr = 1.const / 0.const
//    val exc = intercept[ArithmeticException] {
//      expr.run()
//    }
//    assert(exc.getMessage contains "zero")
//  }
//
//  test("Int - Int") {
//    val expr = 2.const - 1.const
//    val observed = expr.run()
//    assertEquals(
//      observed,
//      Justified.byInference("subtract", 1, NonEmptySeq.of(Justified.byConst(2), Justified.byConst(1))),
//    )
//  }
//
//  test("Int - Int - Int") {
//    val expr = 2.const - 1.const - 1.const
//    val observed = expr.run()
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        0,
//        NonEmptySeq.of(
//          Justified.byInference(
//            "subtract",
//            1,
//            NonEmptySeq.of(
//              Justified.byConst(2),
//              Justified.byConst(1),
//            ),
//          ),
//          Justified.byConst(1),
//        ),
//      ),
//    )
//  }
//
//  test("Long (max) - Int (max)") {
//    val expr = Long.MaxValue.const - Int.MaxValue.const
//    val observed = expr.run()
//    val expected = Long.MaxValue - Int.MaxValue
//    assert(expected > 0, "should be positive")
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(Justified.byConst(Long.MaxValue), Justified.byConst(Int.MaxValue)),
//      ),
//    )
//  }
//
//  test("Int (max) - Long (max)") {
//    val expr = Int.MaxValue.const - Long.MaxValue.const
//    val observed = expr.run()
//    val expected = Int.MaxValue - Long.MaxValue
//    assert(expected < 0, "should be negative")
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(Justified.byConst(Int.MaxValue), Justified.byConst(Long.MaxValue)),
//      ),
//    )
//  }
//
//  test("Double (max) - Int (max)") {
//    val expr = Double.MaxValue.const - Int.MaxValue.const
//    val observed = expr.run()
//    val expected = Double.MaxValue - Int.MaxValue
//    assert(expected > 0, "should be positive")
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(Justified.byConst(Double.MaxValue), Justified.byConst(Int.MaxValue)),
//      ),
//    )
//  }
//
//  test("Int (max) - Double (max)") {
//    val expr = Int.MaxValue.const - Double.MaxValue.const
//    val observed = expr.run()
//    val expected = Int.MaxValue - Double.MaxValue
//    assert(expected < 0, "should be negative")
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(Justified.byConst(Int.MaxValue), Justified.byConst(Double.MaxValue)),
//      ),
//    )
//  }
//
//  test("Float (max) - Int (max)") {
//    val expr = Float.MaxValue.const - Int.MaxValue.const
//    val observed = expr.run()
//    val expected = Float.MaxValue - Int.MaxValue
//    assert(expected > 0, "should be positive")
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(Justified.byConst(Float.MaxValue), Justified.byConst(Int.MaxValue)),
//      ),
//    )
//  }
//
//  test("Int (max) - Float (max)") {
//    val expr = Int.MaxValue.const - Float.MaxValue.const
//    val observed = expr.run()
//    val expected = Int.MaxValue - Float.MaxValue
//    assert(expected < 0, "should be negative")
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(Justified.byConst(Int.MaxValue), Justified.byConst(Float.MaxValue)),
//      ),
//    )
//  }
//
//  test("Double (max) - Float (max)") {
//    val expr = Double.MaxValue.const - Float.MaxValue.const
//    val observed = expr.run()
//    val expected = Double.MaxValue - Float.MaxValue
//    assert(expected > 0, "should be positive")
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(Justified.byConst(Double.MaxValue), Justified.byConst(Float.MaxValue)),
//      ),
//    )
//  }
//
//  test("Float (max) - Double (max)") {
//    val expr = Float.MaxValue.const - Double.MaxValue.const
//    val observed = expr.run()
//    val expected = Float.MaxValue - Double.MaxValue
//    assert(expected < 0, "should be negative")
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(Justified.byConst(Float.MaxValue), Justified.byConst(Double.MaxValue)),
//      ),
//    )
//  }
//
//  test("Instant - Duration") {
//    val now = Instant.now()
//    val duration = Duration.ofMinutes(5)
//    val expr = now.const - duration.const
//    val observed = expr.run()
//    val expected = now.minus(duration)
//    assertEquals(
//      observed,
//      Justified.byInference("subtract", expected, NonEmptySeq.of(Justified.byConst(now), Justified.byConst(duration))),
//    )
//  }
//
//  test("Instant - Duration - Duration") {
//    val now = Instant.now()
//    val duration = Duration.ofMinutes(5)
//    val expr = now.const - duration.const - duration.const
//    val observed = expr.run()
//    val expected = now.minus(duration).minus(duration)
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(
//          Justified.byInference(
//            "subtract",
//            now.minus(duration),
//            NonEmptySeq.of(Justified.byConst(now), Justified.byConst(duration)),
//          ),
//          Justified.byConst(duration),
//        ),
//      ),
//    )
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
//    assert(
//      error contains "try swapping the order of arguments to com.rallyhealth.vapors.v1.data.Justified[java.time.Instant] - com.rallyhealth.vapors.v1.data.Justified[java.time.Duration]",
//    )
//    assert(
//      error contains "you can define an implicit Subtract[com.rallyhealth.vapors.v1.data.Justified[java.time.Duration], com.rallyhealth.vapors.v1.data.Justified[java.time.Instant]]",
//    )
//  }
//
//  test("LocalDate - Period") {
//    val today = LocalDate.now()
//    val period = Period.ofDays(5)
//    val expr = today.const - period.const
//    val observed = expr.run()
//    val expected = today.minus(period)
//    assertEquals(
//      observed,
//      Justified.byInference("subtract", expected, NonEmptySeq.of(Justified.byConst(today), Justified.byConst(period))),
//    )
//  }
//
//  test("LocalDate - Period - Period") {
//    val today = LocalDate.now()
//    val period = Period.ofDays(5)
//    val expr = today.const - period.const - period.const
//    val observed = expr.run()
//    val expected = today.minus(period).minus(period)
//    assertEquals(
//      observed,
//      Justified.byInference(
//        "subtract",
//        expected,
//        NonEmptySeq.of(
//          Justified.byInference(
//            "subtract",
//            today.minus(period),
//            NonEmptySeq.of(Justified.byConst(today), Justified.byConst(period)),
//          ),
//          Justified.byConst(period),
//        ),
//      ),
//    )
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
//    assert(
//      error contains "try swapping the order of arguments to com.rallyhealth.vapors.v1.data.Justified[java.time.LocalDate] - com.rallyhealth.vapors.v1.data.Justified[java.time.Period]",
//    )
//    assert(
//      error contains "you can define an implicit Subtract[com.rallyhealth.vapors.v1.data.Justified[java.time.Period], com.rallyhealth.vapors.v1.data.Justified[java.time.LocalDate]]",
//    )
//  }
//}
