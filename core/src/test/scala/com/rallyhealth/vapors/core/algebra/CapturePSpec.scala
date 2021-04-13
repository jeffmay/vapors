package com.rallyhealth.vapors.core.algebra

import com.rallyhealth.vapors.core.data.Evidence
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.example.{FactTypes, JoeSchmoe, TimeRange}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.wordspec.AnyWordSpec

class CapturePSpec extends AnyWordSpec with TypeCheckedTripleEquals {

  "CaptureP" should {

    "capture a timestamp range post processing param" should {

      import com.rallyhealth.vapors.core.example.CaptureTimeRange._

      "find a single fact from a query" in {
        val q = valuesOfType(FactTypes.WeightMeasurement).exists {
          _.get(_.select(_.value)) > 18
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
