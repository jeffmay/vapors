package com.rallyhealth.vapors.v1

import data.Justified
import lens.DataPath

import cats.data.{NonEmptyList, NonEmptySeq, NonEmptyVector}
import munit.FunSuite

class SimpleJustifiedFilterSpec extends FunSuite {

  import dsl.caching.immutable.justified._

  test("Some[Justified[Int]].filter returns Some[Int]") {
    val input = Some(1)
    val expr = input.const.filter(_ < 3.const)
    val observed = expr.run()
    val expected = Some(Justified.bySelection(1, DataPath.empty.atIndex(0), Justified.byConst(input)))
    assertEquals(observed, expected)
  }

  test("Some[Justified[Int]].filter returns None") {
    val input = Some(1)
    val expr = input.const.filter(_ < 0.const)
    val observed = expr.run()
    assertEquals(observed, None)
  }

  test("None.filter returns None") {
    val expr = none[Int].filter(_ => true.const)
    assertEquals(expr.run(), None)
  }

  test("Seq[Justified[Int]].filter") {
    val input = Seq(1, 2, 3, 4)
    val expr = input.const.filter(_ < 3.const)
    val expected = Justified.elements(Justified.byConst(input)).filter(_.value < 3)
    val res = expr.run()
    assertEquals(res, expected)
  }

  test("Seq[Justified[Nothing]].filter doesn't compile") {
    compileErrors {
      "Seq().const.filter(_ < 3.const)"
    }
  }

  test("NonEmptySeq[Int].filter returns a Seq[Int]") {
    val input = NonEmptySeq.of(1, 2, 3, 4)
    val expr = input.const.filter(_ > 2.const)
    val res = expr.run()
    val justifiedInput = Justified.byConst(input)
    assertEquals(
      res,
      Seq(
        Justified.bySelection(3, DataPath.empty.atIndex(2), justifiedInput),
        Justified.bySelection(4, DataPath.empty.atIndex(3), justifiedInput),
      ),
    )
  }

  test("NonEmptySeq[Int].filter returns an empty Seq[Int]") {
    val expr = NonEmptySeq.of(1, 2, 3, 4).const.filter(_ > 4.const)
    val res = expr.run()
    assertEquals(res, Seq())
  }

  test("NonEmptyList[Int].filter returns a List[Int]") {
    val input = NonEmptyList.of(1, 2, 3, 4)
    val expr = input.const.filter(_ > 2.const)
    val res = expr.run()
    val justifiedInput = Justified.byConst(input)
    assertEquals(
      res,
      List(
        Justified.bySelection(3, DataPath.empty.atIndex(2), justifiedInput),
        Justified.bySelection(4, DataPath.empty.atIndex(3), justifiedInput),
      ),
    )
  }

  test("NonEmptyList[Int].filter returns an empty List[Int]") {
    val expr = NonEmptyList.of(1, 2, 3, 4).const.filter(_ > 4.const)
    val res = expr.run()
    assertEquals(res, Nil)
  }

  test("NonEmptyVector[Int].filter returns a Vector[Int]") {
    val input = NonEmptyVector.of(1, 2, 3, 4)
    val expr = input.const.filter(_ > 2.const)
    val res = expr.run()
    val justifiedInput = Justified.byConst(input)
    assertEquals(
      res,
      Vector(
        Justified.bySelection(3, DataPath.empty.atIndex(2), justifiedInput),
        Justified.bySelection(4, DataPath.empty.atIndex(3), justifiedInput),
      ),
    )
  }

  test("NonEmptyVector[Int].filter returns an empty Vector[Int]") {
    val expr = NonEmptyVector.of(1, 2, 3, 4).const.filter(_ > 4.const)
    val res = expr.run()
    assertEquals(res, Vector())
  }
}
