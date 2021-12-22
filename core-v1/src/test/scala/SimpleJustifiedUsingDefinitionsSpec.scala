package com.rallyhealth.vapors.v1

import data.{FactTable, Justified}
import example.FactTypes

import cats.data.NonEmptySeq
import munit.FunSuite
import org.scalatest.Inside.inside

class SimpleJustifiedUsingDefinitionsSpec extends FunSuite {

  import dsl.simple.justified._

  test("Derive Kg from Lbs") {
    val weightLbs = FactTypes.WeightLbs(150)
    val lbsPerKg = 2.205
    val defineWeightKg = define(FactTypes.WeightKg).from {
      valuesOfType(FactTypes.WeightLbs).map { lbs =>
        lbs / lbsPerKg.const
      }
    }
    val expr = usingDefinitions(defineWeightKg) {
      valuesOfType(FactTypes.WeightKg).map(_ * 2d.const)
    }
    val observedWrapped = expr.run(FactTable(weightLbs))
    inside(observedWrapped) {
      case Seq(Justified.ByInference(reason, observed, justification)) =>
        val expected = (weightLbs.value / lbsPerKg) * 2
        assertEquals(observed, expected)
        assertEquals(reason, "multiply")
        assertEquals(
          justification,
          NonEmptySeq.of(Justified.byFact(FactTypes.WeightKg(68.02721088435374)), Justified.byConst(2.0)),
        )
    }
  }
}
