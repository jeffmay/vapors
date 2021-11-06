package com.rallyhealth.vapors.v1

import data.Justified
import example.NestedSelectable

import cats.data.NonEmptyList
import munit.FunSuite

class JustifiedSelectSpec extends FunSuite {

  import dsl.simple.justified._

  test("select value") {
    val root = NestedSelectable("empty").const
    val expr = root.get(_.select(_.value))
    val expected = Justified.byInference(
      "select", // TODO: Put lens description here?
      root.value.value.value,
      NonEmptyList.of(root.value),
    )
    val obtained = expr.run()
    assertEquals(obtained, expected)
  }
}
