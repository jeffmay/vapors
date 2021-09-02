package com.rallyhealth

package vapors.interpreter

import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class MultiplyOutputsSpec extends AnyFreeSpec {

  "Expr.MultiplyOutputs" - {

    "Int" - {

      "expression multiplied by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ * _,
          const(_) * const(_),
        )
      }

      "value multiplied to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ * _,
          const(_).multiplyTo(_),
        )
      }
    }

    "Double" - {

      "expression multiplied by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
          _ * _,
          const(_) * const(_),
        )
      }

      "value multiplied to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ * _,
          const(_).multiplyTo(_),
        )
      }
    }
  }
}
