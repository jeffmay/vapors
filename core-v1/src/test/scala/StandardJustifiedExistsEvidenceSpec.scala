package com.rallyhealth.vapors.v1

import data.{Evidence, FactTable, Justified, NoEvidence, Window}
import example.FactTypes

import cats.data.NonEmptyList
import munit.FunSuite

class StandardJustifiedExistsEvidenceSpec extends FunSuite {

  import dsl.standard.justified._

  test("Justified[Seq[Int]].exists is false when empty") {
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18.const
    }
    val output = expr.run().state.output
    assertEquals(output, Justified.byConst(false))
    assertEquals(output.evidence, NoEvidence)
  }

  test("Justified[Seq[Int]].exists is false when all false") {
    val age10 = FactTypes.Age(10)
    val age14 = FactTypes.Age(14)
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18.const
    }
    val output = expr.run(FactTable(age10, age14)).state.output
    assertEquals(
      output,
      Justified.byInference(
        "exists",
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
          Justified.byInference(
            "_ >= 18",
            false,
            NonEmptyList.of(
              Justified.byFact(age14),
              Justified.byConst(Window.greaterThanOrEqual(18)),
            ),
          ),
        ),
      ),
    )
    assertEquals(output.evidence, Evidence(age10, age14))
  }

  test("Justified[Seq[Int]].exists is true with a single true result") {
    val age18 = FactTypes.Age(18)
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18.const
    }
    val output = expr.run(FactTable(age18)).state.output
    assertEquals(
      output,
      Justified.byInference(
        "exists",
        true,
        NonEmptyList.of(
          Justified.byInference(
            "_ >= 18",
            true,
            NonEmptyList.of(
              Justified.byFact(age18),
              Justified.byConst(Window.greaterThanOrEqual(18)),
            ),
          ),
        ),
      ),
    )
    assertEquals(output.evidence, Evidence(age18))
  }
}
