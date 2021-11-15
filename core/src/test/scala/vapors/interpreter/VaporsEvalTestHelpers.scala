package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl.{eval, RootExpr}

import org.scalacheck.Arbitrary
import org.scalactic.Tolerance._
import org.scalactic.TripleEqualsSupport._
import org.scalactic.source.Position
import org.scalatest.Assertion
import org.scalatest.Assertions._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

import scala.Fractional.Implicits._
import scala.reflect.ClassTag
import scala.util.Try

object VaporsEvalTestHelpers {

  trait CompareEquality[E, A] {

    def assertEqual(
      expected: E,
      actual: A,
    )(implicit
      pos: Position,
    ): Assertion
  }

  object CompareEquality extends LowPriorityDefaultTolerance {

    def apply[E, A](build: Position => (E, A) => Assertion): CompareEquality[E, A] = new CompareEquality[E, A] {
      override def assertEqual(
        expected: E,
        actual: A,
      )(implicit
        pos: Position,
      ): Assertion = build(pos)(expected, actual)
    }

    implicit def tolerantFractional[A : Fractional : DefaultTolerance]: CompareEquality[A, A] = CompareEquality {
      implicit pos => (expected, actual) =>
        assert(DefaultTolerance[A].toSpread(expected) === actual)
    }

    implicit def tolerantIntegral[A : Integral : DefaultTolerance]: CompareEquality[A, A] = CompareEquality {
      implicit pos => (expected, actual) =>
        assert(DefaultTolerance[A].toSpread(expected) === actual)
    }
  }

  trait LowPriorityDefaultTolerance {

    implicit def zeroToleranceIntegral[A : Integral]: CompareEquality[A, A] = CompareEquality {
      implicit pos => (expected, actual) =>
        assert(expected == actual)
    }
  }

  trait DefaultTolerance[A] {
    def toSpread(value: A): Spread[A]
  }

  object DefaultTolerance {

    @inline def apply[A : DefaultTolerance]: DefaultTolerance[A] = implicitly

    implicit def onePercentFractionalTolerance[A : Fractional]: DefaultTolerance[A] = {
      val F = Fractional[A]
      val onePercent = F.one / F.fromInt(100)
      value => {
        val onePercentOfValue = value * onePercent
        value +- onePercentOfValue
      }
    }
  }

  def producesTheSameResultOrException[A : Arbitrary, B : Arbitrary, R, E <: Throwable : ClassTag](
    evaluate: (A, B) => R,
    buildExpr: (A, B) => RootExpr[R, Unit],
  ): Assertion = {
    forAll { (a: A, b: B) =>
      val query = buildExpr(a, b)
      Try(evaluate(a, b)).fold(
        {
          case expectedExc: E =>
            val exc = intercept[E] {
              eval(FactTable.empty)(query)
            }
            assertResult(expectedExc.getMessage) {
              exc.getMessage
            }
          case unexpected =>
            fail("Unexpected exception", unexpected)
        },
        expectedValue => {
          val result = eval(FactTable.empty)(query)
          expectedValue match {
            case num: Double if num.isNaN =>
              result.output.value match {
                case obs: Double =>
                  assert(obs.isNaN, "Expected output to be NaN")
                case _ =>
                  fail("Expected output to be a Double")
              }
            case _ =>
              assertResult(expectedValue) {
                result.output.value
              }
          }
        },
      )
    }
  }
}
