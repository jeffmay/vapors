package com.rallyhealth

package vapors.interpreter

import vapors.data.Evidence
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe}

import org.scalatest.freespec.AnyFreeSpec

class StandardVaporsEngineSpec extends AnyFreeSpec {

  "StandardVaporsEngine" - {

    "using no post processing" - {

      "find a single fact from a query" in {
        val q = valuesOfType(FactTypes.Age).exists {
          _ >= const(18)
        }
        val result = StandardVaporsEngine.eval(q, JoeSchmoe.factTable).result
        assert(result.param.value === ())
        assert(result.output.value)
        assert(result.output.evidence.nonEmpty)
        assertResult(Evidence(JoeSchmoe.age))(result.output.evidence)
      }

      "find a complex fact from a query" in {
        val likelyToJoinWeightloss = valuesOfType(FactTypes.ProbabilityToUse).exists {
          _.getFoldable(_.select(_.scores).at("weightloss")).exists {
            _ > const(0.5)
          }
        }
        val result = StandardVaporsEngine.eval(likelyToJoinWeightloss, JoeSchmoe.factTable).result
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }
    }
  }
}
