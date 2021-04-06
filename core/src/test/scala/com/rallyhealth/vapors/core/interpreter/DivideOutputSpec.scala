package com.rallyhealth.vapors.core.interpreter

import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.FactTable
import com.rallyhealth.vapors.core.dsl
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.example.FactTypes
import com.rallyhealth.vapors.core.math.Division
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class DivideOutputSpec extends AnyWordSpec {

  s"${classOf[Division[Int]].getSimpleName}[Int]" should {

    lazy val humanAgeFromDogYears: Expr.WithFactsOfType[Int, Seq[Int], Unit] = {
      withFactsOfType(FactTypes.Age).where { facts =>
        facts.map { fact =>
          fact.get(_.select(_.value)) / 7
        }
      }
    }

    "How old is 48 dog years in Human years" in {
      val result = eval(FactTable(FactTypes.Age(48))) {
        humanAgeFromDogYears.withOutputFoldable.exists {
          _ === 6
        }
      }
      assert(result.output.value)
    }

    "How old is 49 dog years in Human years" in {
      val result = eval(FactTable(FactTypes.Age(49))) {
        humanAgeFromDogYears.withOutputFoldable.exists {
          _ === 7
        }
      }
      assert(result.output.value)
    }

    "How old is 50 dog years in Human years" in {
      val result = eval(FactTable(FactTypes.Age(50))) {
        humanAgeFromDogYears.withOutputFoldable.exists {
          _ === 7
        }
      }
      assert(result.output.value)
    }
  }

  s"${classOf[Division[Double]].getSimpleName}[Double]" should {

    lazy val celciusFromFahrenheit: Expr.WithFactsOfType[Double, Seq[Double], Unit] = {
      withFactsOfType(FactTypes.TempFahrenheit).where { facts =>
        facts.map { fact =>
          (fact.get(_.select(_.value)) - 32.0) / 1.8
        }
      }
    }

    "is 32F === 0C (shorter form)" in {
      val result = eval(FactTable(FactTypes.TempFahrenheit(32.0))) {
        celciusFromFahrenheit.withOutputFoldable.exists { v =>
          // TODO Support tolerance here
          // === 0.0 works here, too
          val matchVal = 0.0
          dsl.not(and(v > matchVal, v < matchVal))
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
          dsl.not(and(v > matchVal, v < matchVal))
        }
      }
      assert(result.output.value)
    }

  }
}
