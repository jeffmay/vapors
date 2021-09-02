package com.rallyhealth

package vapors.interpreter

import vapors.algebra.Expr
import vapors.data.FactTable
import vapors.dsl
import vapors.dsl._
import vapors.example.FactTypes

import org.scalatest.freespec.AnyFreeSpec

class DivideOutputsSpec extends AnyFreeSpec {

  "Expr.DivideOutputs" - {

    "Int" - {

      "expression divided by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          _ / _,
          const(_) / const(_),
        )
      }

      "value divided from an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Int, Int, Int, ArithmeticException](
          (a, b) => b / a,
          const(_).divideFrom(_),
        )
      }

      lazy val humanAgeFromDogYears: Expr[FactTable, Seq[Int], Unit] = {
        valuesOfType(FactTypes.Age).map {
          _ / const(7)
        }
      }

      "48 dog years is 6 human years" in {
        val result = eval(FactTable(FactTypes.Age(48))) {
          humanAgeFromDogYears.withOutputFoldable.exists {
            _ === const(6)
          }
        }
        assert(result.output.value)
      }

      "49 dog years is 7 human years" in {
        val result = eval(FactTable(FactTypes.Age(49))) {
          humanAgeFromDogYears.withOutputFoldable.exists {
            _ === const(7)
          }
        }
        assert(result.output.value)
      }

      "50 dog years is 7 human years" in {
        val result = eval(FactTable(FactTypes.Age(50))) {
          humanAgeFromDogYears.withOutputFoldable.exists {
            _ === const(7)
          }
        }
        assert(result.output.value)
      }
    }

    "Double" - {

      "expression divided by an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
          _ / _,
          const(_) / const(_),
        )
      }

      "value divided from an expression" in {
        VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
          (a, b) => b / a,
          const(_).divideFrom(_),
        )
      }

      lazy val celciusFromFahrenheit: Expr[FactTable, Seq[Double], Unit] = {
        valuesOfType(FactTypes.TempFahrenheit).map { tempF =>
          (tempF - const(32.0)) / const(1.8)
        }
      }

      "is 32F === 0C (shorter form)" in {
        val result = eval(FactTable(FactTypes.TempFahrenheit(32.0))) {
          celciusFromFahrenheit.withOutputFoldable.exists { v =>
            // TODO Support tolerance here
            // === 0.0 works here, too
            val matchVal = 0.0
            dsl.not(and(v > const(matchVal), v < const(matchVal)))
          }
        }
        assert(result.output.value)
      }

      "is 50F === 10C (shorter form)" in {
        val result = eval(FactTable(FactTypes.TempFahrenheit(50.0))) {
          celciusFromFahrenheit.withOutputFoldable.exists { v =>
            // TODO support tolerance here
            // === 10.0 works here, too
            val matchVal = 10.0
            dsl.not(and(v > const(matchVal), v < const(matchVal)))
          }
        }
        assert(result.output.value)
      }
    }
  }
}
