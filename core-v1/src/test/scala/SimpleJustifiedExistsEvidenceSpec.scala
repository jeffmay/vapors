package com.rallyhealth.vapors.v1

import data.{Evidence, FactTable, Justified, NoEvidence, Window}
import example.{CombinedTags, FactTypes}
import cats.data.NonEmptySeq
import munit.FunSuite

import scala.collection.immutable.SortedSet

class SimpleJustifiedExistsEvidenceSpec extends FunSuite {

  import dsl.caching.immutable.justified._

  test("Justified[Seq[_]].exists returns all evidence when empty".fail) {
    val emptyTagsFact = FactTypes.CombinedTags(CombinedTags.now(SortedSet()))
    val expr = valuesOfType(FactTypes.CombinedTags).exists {
      _.getAs[Seq](_.select(_.tags)).exists {
        _ === "ignore".const
      }
    }
    val result = expr.run(FactTable(emptyTagsFact))
    assert(!result.value)
    // TODO: This test is marked as failed because the result should contain the original fact as evidence
    assert(result.evidence.factSet.contains(emptyTagsFact))
  }

  test("Justified[Seq[Int]].exists is false when empty") {
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18.const
    }
    val output = expr.run()
    assertEquals(output, Justified.byConst(false))
    assertEquals(output.evidence, NoEvidence)
  }

  test("Justified[Seq[Int]].exists is false when all false results") {
    val age10 = FactTypes.Age(10)
    val age14 = FactTypes.Age(14)
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18.const
    }
    val output = expr.run(FactTable(age10, age14))
    assertEquals(
      output,
      Justified.byInference(
        "exists",
        false,
        NonEmptySeq.of(
          Justified.byInference(
            "_ >= 18",
            false,
            NonEmptySeq.of(
              Justified.byFact(age10),
              Justified.byConst(Window.greaterThanOrEqual(18)),
            ),
          ),
          Justified.byInference(
            "_ >= 18",
            false,
            NonEmptySeq.of(
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
    val output = expr.run(FactTable(age18))
    assertEquals(
      output,
      Justified.byInference(
        "exists",
        true,
        NonEmptySeq.of(
          Justified.byInference(
            "_ >= 18",
            true,
            NonEmptySeq.of(
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
