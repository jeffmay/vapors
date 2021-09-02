package com.rallyhealth

package vapors.interpreter

import vapors.data.Evidence
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe}

import org.scalatest.wordspec.AnyWordSpec

class InterpretExprAsResultFnSpec extends AnyWordSpec {

  "InterpretExprAsFunction" when {

    "using no post processing" should {

      "find a single fact from a query" in {
        val q = valuesOfType(FactTypes.Age).exists {
          _ >= const(18)
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.param.value === ())
        assert(result.output.value)
        assert(result.output.evidence.nonEmpty)
        assertResult(Evidence(JoeSchmoe.age))(result.output.evidence)
      }

      "find a complex fact from a query" in {
        val q = valuesOfType(FactTypes.ProbabilityToUse).exists {
          _.getFoldable(_.select(_.scores).at("weightloss")).exists {
            _ > const(0.5)
          }
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }

      "define a fact expression" in {
        val likelyToJoinWeightloss = valuesOfType(FactTypes.ProbabilityToUse).exists {
          _.getFoldable(_.select(_.scores).at("weightloss")).exists {
            _ > const(0.5)
          }
        }
        val result = eval(JoeSchmoe.factTable)(likelyToJoinWeightloss)
        assertResult(Evidence(JoeSchmoe.probs))(result.output.evidence)
      }
    }
  }
}
