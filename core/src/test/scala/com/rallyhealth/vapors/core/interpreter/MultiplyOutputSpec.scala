package com.rallyhealth.vapors.core.interpreter

import com.rallyhealth.vapors.core.dsl._
import org.scalatest.freespec.AnyFreeSpec

class MultiplyOutputSpec extends AnyFreeSpec {

  "Expr.MultiplyOutput" - {

    "Int" - {

      "expression multiplied by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ * _,
          const(_) * const(_),
        )
      }

      "expression multiplied by a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ * _,
          const(_) * _,
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

      "expression multiplied by a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ * _,
          const(_) * _,
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
