package com.rallyhealth.vapors.v1

import data.{FactTable, Justified, Window}
import example.FactTypes

import cats.data.NonEmptyList
import munit.FunSuite

class StandardJustifiedLogicalSpec extends FunSuite {

  import dsl.standard.justified._

  test("Justified[Seq[Int]].exists is false when empty") {
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18
    }
    val output = expr.run().state.output
    assertEquals(output, Justified.byConst(false))
  }

  test("Justified[Seq[Int]].exists is false when all false") {
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

  test("Justified[Seq[Int]].forall is false when empty") {
    val expr = valuesOfType(FactTypes.Age).exists {
      _ >= 18
    }
    val output = expr.run().state.output
    assertEquals(output, Justified.byConst(false))
  }

  test("Justified[Seq[Int]].forall is true with multiple true results") {
    val age21 = FactTypes.Age(21)
    val age23 = FactTypes.Age(23)
    val expr = valuesOfType(FactTypes.Age).forall {
      _ >= 18
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
  }

  test("Justified[Seq[Int]].forall is false with a single false result") {
    val age10 = FactTypes.Age(10)
    val expr = valuesOfType(FactTypes.Age).forall {
      _ >= 18
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
  }
}
