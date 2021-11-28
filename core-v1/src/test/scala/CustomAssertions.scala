package com.rallyhealth.vapors.v1

import munit.Assertions._
import munit.Location

import scala.util.Try

object CustomAssertions {

  def assertEqualsOrNaN[L, R](
    obtained: L,
    expected: R,
    clue: => Any = "values are not the same",
  )(implicit
    loc: Location,
    ev: R <:< L,
  ): Unit = {
    val lIsNaN = obtained match {
      case l: Double => l.isNaN
      case l: Float => l.isNaN
      case _ => false
    }
    if (lIsNaN) {
      val rIsNaN = expected match {
        case r: Double => r.isNaN
        case r: Float => r.isNaN
        case _ => false
      }
      assert(rIsNaN, s"Expected: NaN\nObtained: $expected ($clue)")
    } else {
      assertEquals(obtained, expected, clue)
    }
  }

  def assertMatchingOutcomes[O](runExpr: => O)(expectedBehavior: => O)(implicit loc: Location): Unit = {
    Try(expectedBehavior).fold({ ex =>
      interceptMessage[Throwable](ex.getMessage) {
        runExpr
      }
    }, { expected =>
      val obtained = runExpr
      assertEqualsOrNaN(obtained, expected)
    })
  }
}
