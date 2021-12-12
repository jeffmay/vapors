package com.rallyhealth.vapors.v1

import data.{Evidence, FactTable, Justified, NoEvidence, Window}
import example.{CombinedTags, FactTypes}

import cats.data.NonEmptyList
import munit.FunSuite

import scala.collection.immutable.SortedSet

class SimpleJustifiedForAllEvidenceSpec extends FunSuite {

  import dsl.simple.justified._

  test("Justified[Seq[_]].forall returns all evidence when empty".fail) {
    val emptyTagsFact = FactTypes.CombinedTags(CombinedTags.now(SortedSet()))
    val expr = valuesOfType(FactTypes.CombinedTags).exists {
      _.getAs[Seq](_.select(_.tags)).forall {
        _ === "ignore".const
      }
    }
    val result = expr.run(FactTable(emptyTagsFact))
    // TODO: This test is marked as failed because the result should contain the original fact as evidence
    assert(result.evidence.factSet.contains(emptyTagsFact))
  }

  test("Justified[Seq[Int]].forall is true when empty") {
    val expr = valuesOfType(FactTypes.Age).forall {
      _ >= 18.const
    }
    val output = expr.run()
    assertEquals(output, Justified.byConst(true))
    assertEquals(output.evidence, NoEvidence)
  }

  test("Justified[Seq[Int]].forall is true when all true results") {
    val age21 = FactTypes.Age(21)
    val age23 = FactTypes.Age(23)
    val expr = valuesOfType(FactTypes.Age).forall {
      _ >= 18.const
    }
    val output = expr.run(FactTable(age21, age23))
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
    val output = expr.run(FactTable(age10))
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
  }
}
