package com.rallyhealth.vapors.core.lens

import com.rallyhealth.vapors.core.example.{BloodPressure, JoeSchmoe}
import org.scalatest.wordspec.AnyWordSpec
import shapeless.{::, HNil, Nat}

import java.time.LocalDate

class NamedLensSpec extends AnyWordSpec {

  "NamedLens.at" should {

    "select a value from an HList using a Nat literal" in {
      val lens = NamedLens.id[Double :: Int :: HNil].at(Nat._1)
      val expected = 1
      assertResult(expected) {
        lens.get(2.0 :: expected :: HNil)
      }
    }

    "fail to compile when using an index out of range" in {
      assertDoesNotCompile {
        "NamedLens.id[Double :: Int :: HNil].at(Nat._2)"
      }
    }
  }

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
