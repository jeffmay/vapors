package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.core.data.Evidence
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.factfilter.Example.{FactTypes, JoeSchmoe}
import org.scalatest.wordspec.AnyWordSpec

class OutputWithinSetExprSpec extends AnyWordSpec {

  "Expr.OutputWithinSet" should {

    "find an asthma tag in a set that contains it" in {
      val q = withFactsOfType(FactTypes.Tag).where {
        _.exists {
          _.value.in(Set("asthma", "diabetes"))
        }
      }
      val result = eval(JoeSchmoe.factTable)(q)
      assert(result.output.value)
      assertResult(Evidence(JoeSchmoe.asthmaTag))(result.output.evidence)
    }

    "not find an asthma tag in a set that does not contain it" in {
      val q = withFactsOfType(FactTypes.Tag).where {
        _.exists {
          _.value.in(Set("diabetes"))
        }
      }
      val result = eval(JoeSchmoe.factTable)(q)
      assert(!result.output.value)
      assertResult(Evidence.none)(result.output.evidence)
    }
  }
}
