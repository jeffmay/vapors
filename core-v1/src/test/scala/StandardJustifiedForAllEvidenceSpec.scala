package com.rallyhealth.vapors.v1

import data.{Evidence, FactTable, Justified, NoEvidence, Window}
import example.FactTypes

import cats.data.NonEmptyList
import munit.FunSuite

class StandardJustifiedForAllEvidenceSpec extends FunSuite {

  import dsl.standard.justified._

  test("Justified[Seq[Int]].forall is true when empty") {
    val expr = valuesOfType(FactTypes.Age).forall {
      _ >= 18.const
    }
    val output = expr.run().state.output
    assertEquals(output, Justified.byConst(true))
    assertEquals(output.evidence, NoEvidence)
  }

  test("Justified[Seq[Int]].forall is true when all true results") {
    val age21 = FactTypes.Age(21)
    val age23 = FactTypes.Age(23)
    val expr = valuesOfType(FactTypes.Age).forall {
      _ >= 18.const
    }
    val output = expr.run(FactTable(age21, age23)).state.output
    assertEquals(
      output,
      Justified.byInference(
        "forall",
        true,
        NonEmptyList.of(
          Justified.byInference(
            "_ >= 18",
            true,
            NonEmptyList.of(
              Justified.byFact(age21),
              Justified.byConst(Window.greaterThanOrEqual(18)),
            ),
          ),
          Justified.byInference(
            "_ >= 18",
            true,
            NonEmptyList.of(
              Justified.byFact(age23),
              Justified.byConst(Window.greaterThanOrEqual(18)),
            ),
          ),
        ),
      ),
    )
    assertEquals(output.evidence, Evidence(age21, age23))
  }

  test("Justified[Seq[Int]].forall is false with a single false result") {
    val age10 = FactTypes.Age(10)
    val expr = valuesOfType(FactTypes.Age).forall {
      _ >= 18.const
    }
    val output = expr.run(FactTable(age10)).state.output
    assertEquals(
      output,
      Justified.byInference(
        "forall",
        false,
        NonEmptyList.of(
          Justified.byInference(
            "_ >= 18",
            false,
            NonEmptyList.of(
              Justified.byFact(age10),
              Justified.byConst(Window.greaterThanOrEqual(18)),
            ),
          ),
        ),
      ),
    )
    assertEquals(output.evidence, Evidence(age10))
  }
}
