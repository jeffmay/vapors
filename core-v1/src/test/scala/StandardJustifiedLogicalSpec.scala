package com.rallyhealth.vapors.v1

import data.{FactTable, Justified, Window}
import example.FactTypes

import cats.data.NonEmptyList
import munit.FunSuite

class StandardJustifiedLogicalSpec extends FunSuite {

  import dsl.standard.justified._

  test("justified expr") {
    val age18 = FactTypes.Age(18)
    val expr = valuesOfType(FactTypes.Age).exists {
      ident[Int] >= 18
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
