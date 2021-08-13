package com.rallyhealth

package vapors.interpreter

import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class SubtractOutputsSpec extends AnyFreeSpec {

  "Expr.SubtractOutputs" - {

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

      "expression subtracted by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ - _,
          const(_) - const(_),
          engine,
        )
      }

      "expression subtracted by a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ - _,
          const(_) - _,
          engine,
        )
      }

      "value subtracted by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          (a, b) => b - a,
          const(_).subtractFrom(_),
          engine,
        )
      }
    }

    "Double" - {

      "expression subtracted by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ - _,
          const(_) - const(_),
          engine,
        )
      }

      "expression subtracted by a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ - _,
          const(_) - _,
          engine,
        )
      }

      "value subtracted by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          (a, b) => b - a,
          const(_).subtractFrom(_),
          engine,
        )
      }
    }
  }
}
