package com.rallyhealth.vapors.v1

import algebra.SizeComparison
import data.Justified

import cats.data.NonEmptySeq
import munit.FunSuite

class SimpleJustifiedSizeIsSpec extends FunSuite {

  import dsl.simple.justified._

  test("Some(1).const.isEmpty == false") {
    val expr = Some(1).const.isEmpty
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 0",
      false,
      NonEmptySeq.of(Justified.byConst(1), Justified.byConst(SizeComparison.===), Justified.byConst(0)),
    )
    assertEquals(observed, expected)
  }

  test("some(1.const).isEmpty == false") {
    val expr = some(1.const).isEmpty
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 0",
      false,
      NonEmptySeq.of(Justified.byConst(1), Justified.byConst(SizeComparison.===), Justified.byConst(0)),
    )
    assertEquals(observed, expected)
  }

  test("none[Any].isEmpty == true") {
    val expr = none[Any].isEmpty
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 0",
      true,
      NonEmptySeq.of(Justified.byConst(0), Justified.byConst(SizeComparison.===), Justified.byConst(0)),
    )
    assertEquals(observed, expected)
  }

  test("Set(1, 2, 3).sizeIs === 3 == true") {
    val expr = Set(1, 2, 3).const.sizeIs === 3.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 3",
      true,
      NonEmptySeq.of(Justified.byConst(3), Justified.byConst(SizeComparison.===), Justified.byConst(3)),
    )
    assertEquals(observed, expected)
  }

  test("Set(1, 2, 3).sizeIs === 2 == false") {
    val expr = Set(1, 2, 3).const.sizeIs === 2.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 2",
      false,
      NonEmptySeq.of(Justified.byConst(3), Justified.byConst(SizeComparison.===), Justified.byConst(2)),
    )
    assertEquals(observed, expected)
  }

  test("Set(1, 2, 3).isEmpty == false") {
    val expr = Set(1, 2, 3).const.isEmpty
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 0",
      false,
      NonEmptySeq.of(Justified.byConst(3), Justified.byConst(SizeComparison.===), Justified.byConst(0)),
    )
    assertEquals(observed, expected)
  }

  test("Set().sizeIs === 1 == false") {
    val expr = Set[Int]().const.sizeIs === 1.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 1",
      false,
      NonEmptySeq.of(Justified.byConst(0), Justified.byConst(SizeComparison.===), Justified.byConst(1)),
    )
    assertEquals(observed, expected)
  }

  test("Set().sizeIs === 0 == true") {
    val expr = Set[Int]().const.sizeIs === 0.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 0",
      true,
      NonEmptySeq.of(Justified.byConst(0), Justified.byConst(SizeComparison.===), Justified.byConst(0)),
    )
    assertEquals(observed, expected)
  }

  test("Set().isEmpty == true") {
    val expr = Set[Int]().const.isEmpty
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 0",
      true,
      NonEmptySeq.of(Justified.byConst(0), Justified.byConst(SizeComparison.===), Justified.byConst(0)),
    )
    assertEquals(observed, expected)
  }

  private val input = Seq(1, 2)
  private val inputExpr = input.const

  test("Justified.elements([1, 2]).isEmpty == false") {
    val expr = inputExpr.isEmpty
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 0",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.===),
        Justified.byConst(0),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([]).isEmpty == true") {
    val expr = Seq.empty[Int].const.isEmpty
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 0",
      true,
      NonEmptySeq.of(
        Justified.byConst(0),
        Justified.byConst(SizeComparison.===),
        Justified.byConst(0),
      ),
    )
    assertEquals(observed, expected)
  }

  // TODO: The thread of justification is broken. It should include the original input const
  // TODO: Test that the justification for the compareTo size is carried through

  test("Justified.elements([1, 2]).sizeIs === 1 == false") {
    val expr = inputExpr.sizeIs === 1.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 1",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.===),
        Justified.byConst(1),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs === 2 == true") {
    val expr = inputExpr.sizeIs === 2.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 2",
      true,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.===),
        Justified.byConst(2),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs === 3 == false") {
    val expr = inputExpr.sizeIs === 3.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs === 3",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.===),
        Justified.byConst(3),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs > 1 == true") {
    val expr = inputExpr.sizeIs > 1.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs > 1",
      true,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.>),
        Justified.byConst(1),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs > 2 == false") {
    val expr = inputExpr.sizeIs > 2.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs > 2",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.>),
        Justified.byConst(2),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs > 3 == false") {
    val expr = inputExpr.sizeIs > 3.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs > 3",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.>),
        Justified.byConst(3),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs >= 1 == true") {
    val expr = inputExpr.sizeIs >= 1.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs >= 1",
      true,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.>=),
        Justified.byConst(1),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs >= 2 == true") {
    val expr = inputExpr.sizeIs >= 2.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs >= 2",
      true,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.>=),
        Justified.byConst(2),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs >= 3 == false") {
    val expr = inputExpr.sizeIs >= 3.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs >= 3",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.>=),
        Justified.byConst(3),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs < 1 == false") {
    val expr = inputExpr.sizeIs < 1.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs < 1",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.<),
        Justified.byConst(1),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs < 2 == false") {
    val expr = inputExpr.sizeIs < 2.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs < 2",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.<),
        Justified.byConst(2),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs < 3 == true") {
    val expr = inputExpr.sizeIs < 3.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs < 3",
      true,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.<),
        Justified.byConst(3),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs <= 1 == false") {
    val expr = inputExpr.sizeIs < 1.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs < 1",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.<),
        Justified.byConst(1),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs <= 2 == false") {
    val expr = inputExpr.sizeIs < 2.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs < 2",
      false,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.<),
        Justified.byConst(2),
      ),
    )
    assertEquals(observed, expected)
  }

  test("Justified.elements([1, 2]).sizeIs <= 3 == true") {
    val expr = inputExpr.sizeIs < 3.const
    val observed = expr.run()
    val expected = Justified.byInference(
      "sizeIs < 3",
      true,
      NonEmptySeq.of(
        Justified.byConst(2),
        Justified.byConst(SizeComparison.<),
        Justified.byConst(3),
      ),
    )
    assertEquals(observed, expected)
  }
}
