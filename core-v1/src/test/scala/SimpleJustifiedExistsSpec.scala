package com.rallyhealth.vapors.v1

import data.{FactTable, Justified, Window}
import example.{CombinedTags, FactTypes}

import cats.data.NonEmptyList

import scala.collection.immutable.SortedSet

class SimpleJustifiedExistsSpec extends munit.FunSuite {

  import dsl.simple.justified._

  test("Justified[Seq[_]].exists returns all evidence when empty".fail) {
    val emptyTagsFact = FactTypes.CombinedTags(CombinedTags.now(SortedSet()))
    val expr = valuesOfType(FactTypes.CombinedTags).exists {
      _.get(_.select(_.tags)).exists {
        _ === "ignore"
      }
    }
    val result = expr.run(FactTable(emptyTagsFact))
    assert(!result.value)
    // TODO: This should contain the original fact as evidence
    assert(result.evidence.factSet.contains(emptyTagsFact))
  }

  test("Justified[Seq[Int]].exists is false when empty") {
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18
    }
    val output = expr.run()
    assertEquals(output, Justified.byConst(false))
  }

  test("Justified[Seq[Int]].exists is false when all false results") {
    val age10 = FactTypes.Age(10)
    val age14 = FactTypes.Age(14)
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18
    }
    val output = expr.run(FactTable(age10, age14))
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
              Justified.byFact(age14),
              Justified.byConst(Window.greaterThanOrEqual(18)),
            ),
          ),
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

  test("Justified[Seq[Int]].exists is true with a single true result") {
    val age18 = FactTypes.Age(18)
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18
    }
    val output = expr.run(FactTable(age18))
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
  }
}
