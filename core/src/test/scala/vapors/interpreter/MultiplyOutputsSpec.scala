package com.rallyhealth

package vapors.interpreter

import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class MultiplyOutputsSpec extends AnyFreeSpec {

  "Expr.MultiplyOutputs" - {

    "standard engine" - {
      allTests(StandardVaporsEngine)
    }

    "cats effect engine" - {
      import cats.effect.unsafe.implicits.global
      allTests(CatsEffectSimpleVaporsEngine)
    }
  }

  private def allTests[F[_]](
    engine: VaporsEngine[F, Unit],
  )(implicit
    engineExtractParam: engine.ExtractParam,
  ): Unit = {

    "Int" - {

      "expression multiplied by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ * _,
          const(_) * const(_),
          engine,
        )
      }

      "expression multiplied by a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ * _,
          const(_) * _,
          engine,
        )
      }

      "value multiplied to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ * _,
          const(_).multiplyTo(_),
          engine,
        )
      }
    }

    "Double" - {

      "expression multiplied by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ * _,
          const(_) * const(_),
          engine,
        )
      }

      "expression multiplied by a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ * _,
          const(_) * _,
          engine,
        )
      }

      "value multiplied to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ * _,
          const(_).multiplyTo(_),
          engine,
        )
      }
    }
  }
}
