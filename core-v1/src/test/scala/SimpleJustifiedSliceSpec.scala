package com.rallyhealth.vapors.v1

import data.Justified

import munit.FunSuite

class SimpleJustifiedSliceSpec extends FunSuite {

  import dsl.simple.justified._

  test("Seq[Justified[Int]](1..5).slice(1 <-> 3)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(1 <-> 3)
    val observed = expr.run()
    val expected = Justified.elements(Justified.byConst(input)).slice(1, 3)
    assertEquals(observed, expected)
  }

  test("Seq[Justified[Int]](1..5).slice(-3 <-> -1)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(-3 <-> -1)
    val observed = expr.run()
    val expected = Justified.elements(Justified.byConst(input)).slice(2, 4)
    assertEquals(observed, expected)
  }

  test("Seq[Justified[Int]](1..5).slice(1 <-> -1)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(1 <-> -1)
    val observed = expr.run()
    val expected = Justified.elements(Justified.byConst(input)).slice(1, 4)
    assertEquals(observed, expected)
  }

  test("Seq[Justified[Int]](1..5).slice(2 <-> End)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(2 <-> End)
    val observed = expr.run()
    val expected = Justified.elements(Justified.byConst(input)).slice(2, 5)
    assertEquals(observed, expected)
  }

  test("Seq[Justified[Int]](1..5).slice(-2 <-> End)") {
    val input = Seq(1, 2, 3, 4, 5)
    val expr = input.const.slice(-2 <-> End)
    val observed = expr.run()
    val expected = Justified.elements(Justified.byConst(input)).slice(3, 5)
    assertEquals(observed, expected)
  }
}
