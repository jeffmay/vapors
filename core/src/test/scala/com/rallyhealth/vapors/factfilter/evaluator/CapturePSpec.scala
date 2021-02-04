package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.core.data.Evidence
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.example.TimeRange
import com.rallyhealth.vapors.factfilter.Example.{FactTypes, JoeSchmoe}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.wordspec.AnyWordSpec

class CapturePSpec extends AnyWordSpec with TypeCheckedTripleEquals {

  "CaptureP" should {

    "capture a timestamp range post processing param" should {

      import com.rallyhealth.vapors.core.example.CaptureTimeRange._

      "find a single fact from a query" in {
        val q = withFactsOfType(FactTypes.WeightMeasurement).where {
          _.exists {
            _.get(_.select(_.value).select(_.value)) > 18
          }
        }
        val result = eval(JoeSchmoe.factTable)(q)
        assert(result.param.value === TimeRange(JoeSchmoe.weight.value.timestamp))
        assert(result.output.value)
        assert(result.output.evidence.nonEmpty)
        assertResult(Evidence(JoeSchmoe.weight))(result.output.evidence)
      }
    }
  }
}
