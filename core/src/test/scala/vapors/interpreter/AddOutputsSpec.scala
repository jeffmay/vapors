package com.rallyhealth

package vapors.interpreter

import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class AddOutputsSpec extends AnyFreeSpec {

  "Expr.AddOutputs" - {

    "standard eval" - {
      allTests(StandardVaporsEngine)
    }

    "fast eval" - {
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

      "expression added to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ + _,
          const(_) + const(_),
          engine,
        )
      }

      "value added to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ + _,
          const(_) + _,
          engine,
        )
      }

      "expression added to a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ + _,
          const(_).addTo(_),
          engine,
        )
      }
    }

    "Double" - {

      "expression added to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ + _,
          const(_) + const(_),
          engine,
        )
      }

      "value added to an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ + _,
          const(_) + _,
          engine,
        )
      }

      "expression added to a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ + _,
          const(_).addTo(_),
          engine,
        )
      }
    }
  }
}
