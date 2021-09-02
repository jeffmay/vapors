package com.rallyhealth

package vapors.interpreter

import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class SubtractOutputsSpec extends AnyFreeSpec {

  "Expr.SubtractOutputs" - {

    "Int" - {

      "expression subtracted by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ - _,
          const(_) - const(_),
        )
      }

      "value subtracted by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          (a, b) => b - a,
          const(_).subtractFrom(_),
        )
      }
    }

    "Double" - {

      "expression subtracted by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
          _ - _,
          const(_) - const(_),
        )
      }

      "value subtracted by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
          (a, b) => b - a,
          const(_).subtractFrom(_),
        )
      }
    }
  }
}
