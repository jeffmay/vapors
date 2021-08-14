package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl._
import vapors.example.{BloodPressure, FactTypes}

import org.scalatest.freespec.AnyFreeSpec

import java.time.Instant

class SortOutputSpec extends AnyFreeSpec {

  "Expr.SortOutput" - {

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

    val bpNow = Instant.now()
    val bp5MinAgo = bpNow.minusSeconds(5 * 60)
    val bp15MinAgo = bpNow.minusSeconds(15 * 60)
    val lowDiastolic = BloodPressure(70, 100, bp15MinAgo)
    val medDiastolic = BloodPressure(80, 100, bp5MinAgo)
    val highDiastolic = BloodPressure(90, 100, bpNow)

    val bpFacts = FactTable(
      Seq(
        lowDiastolic,
        medDiastolic,
        highDiastolic,
      ).map(FactTypes.BloodPressureMeasurement(_)),
    )

    "sorted using natural ordering" in {
      val query = valuesOfType(FactTypes.BloodPressureMeasurement).map(_.get(_.select(_.diastolic))).sorted
      assertResult(Some(highDiastolic)) {
        bpFacts.getSortedSeq(FactTypes.BloodPressureMeasurement).headOption.map(_.value)
      }
      val result = engine.evalAndExtractValue(query, bpFacts)
      assertResult(lowDiastolic.diastolic) {
        result.head
      }
    }

    "sortBy ordered field" in {
      val query = valuesOfType(FactTypes.BloodPressureMeasurement).sortBy(_.select(_.diastolic))
      assertResult(Some(highDiastolic)) {
        bpFacts.getSortedSeq(FactTypes.BloodPressureMeasurement).headOption.map(_.value)
      }
      val result = engine.evalAndExtractValue(query, bpFacts)
      assertResult(lowDiastolic) {
        result.head
      }
    }
  }
}
