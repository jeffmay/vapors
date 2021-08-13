package com.rallyhealth

package vapors.interpreter

import vapors.data.Evidence
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe}

import org.scalatest.freespec.AnyFreeSpec

class OutputWithinSetExprSpec extends AnyFreeSpec {

  "Expr.OutputWithinSet" - {

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

    "find an asthma tag in a set that contains it" in {
      val q = valuesOfType(FactTypes.Tag).exists {
        _ in Set("asthma", "diabetes")
      }
      val result = engine.eval(q, JoeSchmoe.factTable)
      val resultValue = engine.extract(result.value)
      assert(resultValue)
      for (evidence <- result.maybeEvidence) {
        assertResult(Evidence(JoeSchmoe.asthmaTag)) {
          engine.extract(evidence)
        }
      }
    }

    "not find an asthma tag in a set that does not contain it" in {
      val q = valuesOfType(FactTypes.Tag).exists {
        _ in Set("diabetes")
      }
      val result = engine.eval(q, JoeSchmoe.factTable)
      val resultValue = engine.extract(result.value)
      assert(!resultValue)
      for (evidence <- result.maybeEvidence) {
        assertResult(Evidence.none) {
          engine.extract(evidence)
        }
      }
    }
  }
}
