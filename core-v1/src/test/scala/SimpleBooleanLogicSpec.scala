package com.rallyhealth.vapors.v1

import algebra.Expr

import munit.{FunSuite, Location}

class SimpleBooleanLogicSpec extends FunSuite {

  import dsl.simple._

  private def testLogic(
    l: Any ~:> Boolean,
    r: Any ~:> Boolean,
    expected: Boolean,
  )(
    combine: (Any ~:> Boolean, Any ~:> Boolean) => Any ~:> Boolean,
  )(implicit
    loc: Location,
  ): Unit = {
    val obtained = combine(l, r).run()
    assertEquals(obtained, expected)
  }

  private def test_&&(
    l: Expr.Const[Boolean, OP],
    r: Expr.Const[Boolean, OP],
  )(implicit
    loc: Location,
  ): Unit = testLogic(l, r, l.value && r.value)(_ && _)

  test("true && true == true") {
    test_&&(true.const, true.const)
  }

  test("true && false == false") {
    test_&&(true.const, false.const)
  }

  test("false && true == false") {
    test_&&(false.const, true.const)
  }

  test("false && false == false") {
    test_&&(false.const, false.const)
  }

  private def test_and(
    l: Expr.Const[Boolean, OP],
    r: Expr.Const[Boolean, OP],
  )(implicit
    loc: Location,
  ): Unit = testLogic(l, r, l.value && r.value)(and(_, _))

  test("and(true, true) == true") {
    test_and(true.const, true.const)
  }

  test("and(true, false) == false") {
    test_and(true.const, false.const)
  }

  test("and(false, true) == false") {
    test_and(false.const, true.const)
  }

  test("and(false, false) == false") {
    test_and(false.const, false.const)
  }

  private def test_||(
    l: Expr.Const[Boolean, OP],
    r: Expr.Const[Boolean, OP],
  )(implicit
    loc: Location,
  ): Unit = testLogic(l, r, l.value || r.value)(_ || _)

  test("true || true == true") {
    test_||(true.const, true.const)
  }

  test("true || false == true") {
    test_||(true.const, false.const)
  }

  test("false || false == true") {
    test_||(false.const, true.const)
  }

  test("false || false == false") {
    test_||(false.const, false.const)
  }

  private def test_or(
    l: Expr.Const[Boolean, OP],
    r: Expr.Const[Boolean, OP],
  )(implicit
    loc: Location,
  ): Unit = testLogic(l, r, l.value || r.value)(or(_, _))

  test("or(true, true) == true") {
    test_or(true.const, true.const)
  }

  test("or(true, false) == true") {
    test_or(true.const, false.const)
  }

  test("or(false, true) == true") {
    test_or(false.const, true.const)
  }

  test("or(false, false) == false") {
    test_or(false.const, false.const)
  }

  test("!true == false") {
    val obtained = !true.const.run()
    assertEquals(obtained, false)
  }

  test("!false == true") {
    val obtained = !false.const.run()
    assertEquals(obtained, true)
  }

  test("not(true) == false") {
    val obtained = not(true.const).run()
    assertEquals(obtained, false)
  }

  test("not(false) == true") {
    val obtained = not(false.const).run()
    assertEquals(obtained, true)
  }
}
