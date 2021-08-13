package com.rallyhealth

package vapors.interpreter

import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class DivideOutputsSpec extends AnyFreeSpec {

  "Expr.DivideOutputs" - {

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

      "expression divided by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ / _,
          const(_) / const(_),
          engine,
        )
      }

      "expression divided by a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          _ / _,
          const(_) / _,
          engine,
        )
      }

      "value divided from an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Int, Int, Int, ArithmeticException](
          (a, b) => b / a,
          const(_).divideFrom(_),
          engine,
        )
      }

      def humanAgeFromDogYears(humanAge: Int): RootExpr[Int, Unit] = {
        const(humanAge) / const(7)
      }

      "48 dog years is 6 human years" in {
        val result = engine.evalAndExtractValue(humanAgeFromDogYears(48))
        assertResult(6)(result)
      }

      "49 dog years is 7 human years" in {
        val result = engine.evalAndExtractValue(humanAgeFromDogYears(49))
        assertResult(7)(result)
      }

      "50 dog years is 7 human years" in {
        val result = engine.evalAndExtractValue(humanAgeFromDogYears(50))
        assertResult(7)(result)
      }
    }

    "Double" - {

      "expression divided by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ / _,
          const(_) / const(_),
          engine,
        )
      }

      "expression divided by a value" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          _ / _,
          const(_) / _,
          engine,
        )
      }

      "value divided from an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[F, Double, Double, Double, ArithmeticException](
          (a, b) => b / a,
          const(_).divideFrom(_),
          engine,
        )
      }

      def celciusFromFahrenheit(degreesFahrenheit: Double): RootExpr[Double, Unit] = {
        (const(degreesFahrenheit) - const(32.0)) / const(1.8)
      }

      "is 32F === 0C (shorter form)" in {
        val result = engine.evalAndExtractValue(celciusFromFahrenheit(32.0))
        assertResult(0.0)(result)
      }

      "is 50F === 10C (shorter form)" in {
        val result = engine.evalAndExtractValue(celciusFromFahrenheit(50.0))
        assertResult(10.0)(result)
      }
    }
  }
}
