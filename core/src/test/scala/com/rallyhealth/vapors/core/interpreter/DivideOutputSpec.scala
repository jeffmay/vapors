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

  s"${classOf[Division[Double]].getSimpleName}[Double]" should {

    lazy val celciusFromFahrenheit: Expr.WithFactsOfType[Double, Seq[Double], Unit] = {
      withFactsOfType(FactTypes.TempFahrenheit).where { facts =>
        facts.map { fact =>
          (fact.get(_.select(_.value)) - 32.0) / 1.8
        }
      }
    }

    "is 32F === 0C (shorter form)" in {
      val tempZeroF = FactTypes.TempFahrenheit(32.0)
      val facts = FactTable(tempZeroF)
      val result = eval(facts) {
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
      val tempZeroF = FactTypes.TempFahrenheit(50.0)
      val facts = FactTable(tempZeroF)
      val result = eval(facts) {
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
