package com.rallyhealth.vapors.core.lens

import com.rallyhealth.vapors.core.example.{BloodPressure, JoeSchmoe}
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class NamedLensSpec extends AnyWordSpec {

  "NamedLens.select" should {

    "select a case class field" in {
      val lens = NamedLens.id[BloodPressure].select(_.diastolic)
      assertResult(DataPath(List(DataPath.Field("diastolic"))))(lens.path)
      assertResult(JoeSchmoe.bloodPressure.value.diastolic) {
        lens.get(JoeSchmoe.bloodPressure.value)
      }
    }

    "select a Java bean style getter method" in {
      val lens = NamedLens.id[LocalDate].select(_.getYear)
      // TODO: Should this remove the "get" and lowercase the first letter?
      assertResult(DataPath(List(DataPath.Field("getYear"))))(lens.path)
      assertResult(JoeSchmoe.dateOfBirth.value.getYear) {
        lens.get(JoeSchmoe.dateOfBirth.value)
      }
    }
  }
}
