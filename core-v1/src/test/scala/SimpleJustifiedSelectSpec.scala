package com.rallyhealth.vapors.v1

import data.Justified
import example.NestedSelectable

import cats.data.NonEmptyList
import munit.FunSuite

class SimpleJustifiedSelectSpec extends FunSuite {

  import dsl.simple.justified._

  import NestedSelectable.empty

  test("select value") {
    val root = empty.const
    val expr = root.get(_.select(_.value))
    val expected = Justified.byInference(
      "elementOf", // TODO: Put lens description here?
      root.value.value.value,
      NonEmptyList.of(root.value),
    )
    val obtained = expr.run()
    assertEquals(obtained, expected)
  }

  test("select optional value") {
    val fixture = NestedSelectable("optional", Some(empty))
    val root = NestedSelectable("optional", Some(empty)).const
    val expr = root.get(_.select(_.opt)).map {
      _.get(_.select(_.value))
    }
    val expected = Some(
      Justified.byInference(
        "elementOf",
        "empty",
        NonEmptyList.of(
          Justified.byInference(
            "elementOf",
            empty,
            NonEmptyList.of(Justified.byConst(fixture)),
          ),
        ),
      ),
    )
    val obtained = expr.run()
    assertEquals(obtained, expected)
  }
}
