package com.rallyhealth.vapors.v1

import data.{FactTable, Justified, Window}
import example.FactTypes

import cats.data.NonEmptyList
import munit.FunSuite

class StandardJustifiedExistsSpec extends FunSuite {

  import dsl.standard.justified._

  test("Justified[Seq[Int]].exists is false when empty") {
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18
    }
    val output = expr.run().state.output
    assertEquals(output, Justified.byConst(false))
  }

  test("Justified[Seq[Int]].exists is false when all false results") {
    val age10 = FactTypes.Age(10)
    val age14 = FactTypes.Age(14)
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18
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
  }
}
