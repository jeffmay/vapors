package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleSizeIsSpec extends FunSuite {

  import dsl.simple._

  test("Some(1).const.isEmpty == false") {
    val expr = Some(1).const.isEmpty
    assert(!expr.run())
  }

  test("some(1.const).isEmpty == false") {
    val expr = some(1.const).isEmpty
    assert(!expr.run())
  }

  test("none[Any].isEmpty == true") {
    val expr = none[Any].isEmpty
    assert(expr.run())
  }

  test("Set(1, 2, 3).sizeIs === 3 == true") {
    val expr = Set(1, 2, 3).const.sizeIs === 3.const
    assert(expr.run())
  }

  test("Set(1, 2, 3).sizeIs === 2 == false") {
    val expr = Set(1, 2, 3).const.sizeIs === 2.const
    assert(!expr.run())
  }

  test("Set(1, 2, 3).isEmpty == false") {
    val expr = Set(1, 2, 3).const.isEmpty
    assert(!expr.run())
  }

  test("Set().sizeIs == 1 == false") {
    val expr = Set[Int]().const.sizeIs === 1.const
    assert(!expr.run())
  }

  test("Set().sizeIs == 0 == true") {
    val expr = Set[Int]().const.sizeIs === 0.const
    assert(expr.run())
  }

  test("Set().isEmpty == true") {
    val expr = Set[Int]().const.isEmpty
    assert(expr.run())
  }

  private val inputExpr = Seq(1, 2).const

  test("[1, 2].isEmpty == false") {
    val expr = inputExpr.isEmpty
    assert(!expr.run())
  }

  test("[].isEmpty == true") {
    val expr = Seq.empty[Int].const.isEmpty
    assert(expr.run())
  }

  test("[1, 2].sizeIs === 1 == false") {
    val expr = inputExpr.sizeIs === 1.const
    assert(!expr.run())
  }

  test("[1, 2].sizeIs === 2 == true") {
    val expr = inputExpr.sizeIs === 2.const
    assert(expr.run())
  }

  test("[1, 2].sizeIs === 3 == false") {
    val expr = inputExpr.sizeIs === 3.const
    assert(!expr.run())
  }

  test("[1, 2].sizeIs > 1 == true") {
    val expr = inputExpr.sizeIs > 1.const
    assert(expr.run())
  }

  test("[1, 2].sizeIs > 2 == false") {
    val expr = inputExpr.sizeIs > 2.const
    assert(!expr.run())
  }

  test("[1, 2].sizeIs > 3 == false") {
    val expr = inputExpr.sizeIs > 3.const
    assert(!expr.run())
  }

  test("[1, 2].sizeIs >= 1 == true") {
    val expr = inputExpr.sizeIs >= 1.const
    assert(expr.run())
  }

  test("[1, 2].sizeIs >= 2 == true") {
    val expr = inputExpr.sizeIs >= 2.const
    assert(expr.run())
  }

  test("[1, 2].sizeIs >= 3 == false") {
    val expr = inputExpr.sizeIs >= 3.const
    assert(!expr.run())
  }

  test("[1, 2].sizeIs < 1 == false") {
    val expr = inputExpr.sizeIs < 1.const
    assert(!expr.run())
  }

  test("[1, 2].sizeIs < 2 == false") {
    val expr = inputExpr.sizeIs < 2.const
    assert(!expr.run())
  }

  test("[1, 2].sizeIs < 3 == true") {
    val expr = inputExpr.sizeIs < 3.const
    assert(expr.run())
  }

  test("[1, 2].sizeIs <= 1 == false") {
    val expr = inputExpr.sizeIs <= 1.const
    assert(!expr.run())
  }

  test("[1, 2].sizeIs <= 2 == true") {
    val expr = inputExpr.sizeIs <= 2.const
    assert(expr.run())
  }

  test("[1, 2].sizeIs <= 3 == true") {
    val expr = inputExpr.sizeIs <= 3.const
    assert(expr.run())
  }
}
