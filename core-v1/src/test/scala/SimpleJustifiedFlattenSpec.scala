package com.rallyhealth.vapors.v1

import data.Justified
import lens.DataPath

import munit.FunSuite

class SimpleJustifiedFlattenSpec extends FunSuite {

  import dsl.uncached.justified._

  def alphaAt[C[_]](n: Int): Char = ('A' + n).toChar

  private val nestedSeqLetters = Seq(0, 1, 2).map(i => Seq.fill(i + 1)(alphaAt(i)))
  private val justifiedConst = Justified.byConst(nestedSeqLetters)
  private val expectedFlattened = Seq(
    Justified.BySelection('A', DataPath.empty.atIndex(0).atIndex(0), justifiedConst),
    Justified.BySelection('B', DataPath.empty.atIndex(1).atIndex(0), justifiedConst),
    Justified.BySelection('B', DataPath.empty.atIndex(1).atIndex(1), justifiedConst),
    Justified.BySelection('C', DataPath.empty.atIndex(2).atIndex(0), justifiedConst),
    Justified.BySelection('C', DataPath.empty.atIndex(2).atIndex(1), justifiedConst),
    Justified.BySelection('C', DataPath.empty.atIndex(2).atIndex(2), justifiedConst),
  )

  test("List[List[Justified[Char]]].flatten infers a List") {
    val values: List[List[Char]] = nestedSeqLetters.map(_.toList).toList
    val expr: Any ~:> List[Justified[Char]] = values.const.flatten
    val observed = expr.run()
    assertEquals(observed, expectedFlattened.toList)
  }

  test("Seq[List[Char]].flatten infers a Seq") {
    val values: Seq[List[Char]] = nestedSeqLetters.map(_.toList)
    val expr: Any ~:> Seq[Justified[Char]] = values.const.flatten[Seq]
    val observed = expr.run()
    assertEquals(observed, expectedFlattened)
  }

  test("List[Seq[Char]].flatten[Seq] compiles when explicit") {
    val values: List[Seq[Char]] = nestedSeqLetters.toList
    val expr: Any ~:> Seq[Justified[Char]] = values.const.flatten[Seq]
    val observed = expr.run()
    assertEquals(observed, expectedFlattened)
  }

  test("Vector[List[Char]].flatten[Seq] compiles when explicit") {
    val values: Vector[List[Char]] = nestedSeqLetters.map(_.toList).toVector
    val expr: Any ~:> Seq[Justified[Char]] = values.const.flatten[Seq]
    val observed = expr.run()
    assertEquals(observed, expectedFlattened)
  }

  test("Vector[Int].flatMap(Seq[Int])") {
    val values = Vector(1, 2)
    val innerValues = Seq(3, 4)
    val expr = values.const.flatMap { _ =>
      innerValues.const
    }
    val observed = expr.run()
    val expected = values.flatMap(_ => Justified.elements(Justified.byConst(innerValues)))
    assertEquals(observed, expected)
  }

  test("Seq[Int].flatMap(Vector[Int])") {
    val values = Seq(1, 2, 3)
    val innerValues = Vector(2, 3, 4)
    val expr = values.const.flatMap { _ =>
      innerValues.const
    }
    val observed = expr.run()
    val expected = values.flatMap(_ => Justified.elements(Justified.byConst(innerValues)))
    assertEquals(observed, expected)
  }

  test("List[Int].flatMap[Seq, Int](Vector[Int]) compiles when explicit") {
    val values = List(1, 2, 3)
    val innerValues = Vector(2, 3, 4)
    val expr = values.const.flatMap[Seq, Int] { _ =>
      innerValues.const
    }
    val observed = expr.run()
    val expected = values.flatMap(_ => Justified.elements(Justified.byConst(innerValues)))
    assertEquals(observed, expected)
  }
}
