package com.rallyhealth.vapors.v1

import CustomAssertions.{assertEqualsOrNaN, assertMatchingOutcomes}
import cats.data.NonEmptySeq
import com.rallyhealth.vapors.v1.data.Justified
import munit.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class SimpleJustifiedPowerSpec extends FunSuite {

  import dsl.uncached.justified._

  test("Int ^ Int (behavior)") {
    forAll { (l: Int, r: Int) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }

  test("pow(Int, Int) (syntax)") {
    val expr = pow(3.const, 2.const)
    val observed = expr.run()
    assertEqualsOrNaN(observed.value, 9d)
  }

  test("Long ^ Int (behavior)") {
    forAll { (l: Long, r: Int) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }

  test("pow(Long, Int) (syntax)") {
    val expr = pow(3L.const, 2.const)
    val observed = expr.run()
    assertEqualsOrNaN(observed.value, 9d)
  }

  test("Int ^ Long (behavior)") {
    forAll { (l: Int, r: Long) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }

  test("pow(Int, Long) (syntax)") {
    val expr = pow(3.const, 2L.const)
    val observed = expr.run()
    assertEquals(observed.value, 9d)
  }

  test("Double ^ Int (behavior)") {
    forAll { (l: Double, r: Int) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }

  test("pow(Double, Int) (syntax)") {
    val expr = pow(3d.const, 2.const)
    val observed = expr.run()
    assertEquals(observed.value, 9d)
  }

  test("Int ^ Double (behavior)") {
    forAll { (l: Int, r: Double) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }

  test("pow(Int, Double) (syntax)") {
    val expr = pow(3.const, 2d.const)
    val observed = expr.run()
    assertEquals(observed.value, 9d)
  }

  test("Double ^ Float (behavior)") {
    forAll { (l: Double, r: Float) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }

  test("pow(Double, Float) (syntax)") {
    val expr = pow(3d.const, 2f.const)
    val observed = expr.run()
    assertEquals(observed.value, 9d)
  }

  test("Float ^ Double (behavior)") {
    forAll { (l: Double, r: Float) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }

  test("pow(Float, Double) (syntax)") {
    val expr = pow(3f.const, 2d.const)
    val observed = expr.run()
    assertEquals(observed.value, 9d)
  }

  test("Long ^ Float (behavior)") {
    forAll { (l: Long, r: Float) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }
  test("pow(Long, Float) (syntax)") {
    val expr = pow(3L.const, 2f.const)
    val observed = expr.run()
    assertEquals(observed.value, 9d)
  }

  test("Float ^ Long (behavior)") {
    forAll { (l: Float, r: Long) =>
      val expr = l.const ^ r.const
      val expected = Math.pow(l, r)
      val observed = expr.run()
      observed match {
        case Justified.ByInference(reason, value, sources) =>
          assertEqualsOrNaN(value, expected)
          assertEquals(reason, "power")
          assertEquals(
            sources,
            NonEmptySeq.of(
              Justified.byConst(l),
              Justified.byConst(r),
            ),
          )
        case _ =>
          fail("Expected a Justified.ByInference result")
      }
    }
  }

  test("pow(Float, Long) (syntax)") {
    val expr = pow(3f.const, 2L.const)
    val observed = expr.run()
    assertEquals(observed.value, 9d)
  }
}
