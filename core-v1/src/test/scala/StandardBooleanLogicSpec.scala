package com.rallyhealth.vapors.v1

import algebra.Expr

import cats.data.NonEmptyVector
import munit.{FunSuite, Location}

class StandardBooleanLogicSpec extends FunSuite {

  import dsl.standard._

  private def testLogic(
    head: Expr.Const[Boolean, OP],
    tail: NonEmptyVector[Expr.Const[Boolean, OP]],
  )(
    expect: NonEmptyVector[Boolean] => Boolean,
  )(
    combine: (Any ~:> Boolean, NonEmptyVector[Any ~:> Boolean]) => Any ~:> Boolean,
  )(implicit
    loc: Location,
  ): Unit = {
    val obtained = combine(head, tail).run()
    val expected = expect((head +: tail).map(_.value))
    assertEquals(obtained.state.output, expected)
  }

  private def test_&&(
    l: Expr.Const[Boolean, OP],
    r: Expr.Const[Boolean, OP],
    m: Expr.Const[Boolean, OP]*,
  )(implicit
    loc: Location,
  ): Unit = {
    val tail = NonEmptyVector(r, m.toVector)
    testLogic(l, tail) {
      _.reduceLeft {
        _ && _
      }
    } { (h, t) =>
      t.foldLeft(h) {
        _ && _
      }
    }
  }

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

  test("true && true && true == true") {
    test_&&(true.const, true.const, true.const)
  }

  test("true && true && false == false") {
    test_&&(true.const, true.const, false.const)
  }

  test("false && false && true == false") {
    test_&&(false.const, false.const, true.const)
  }

  test("false && false && false == false") {
    test_&&(false.const, false.const, false.const)
  }

  private def test_and(
    l: Expr.Const[Boolean, OP],
    r: Expr.Const[Boolean, OP],
    m: Expr.Const[Boolean, OP]*,
  )(implicit
    loc: Location,
  ): Unit = {
    val tail = NonEmptyVector(r, m.toVector)
    testLogic(l, tail) {
      _.reduceLeft {
        _ && _
      }
    } { (h, t) =>
      and(h, t.head, t.tail: _*)
    }
  }

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

  test("and(true, true, true) == true") {
    test_and(true.const, true.const, true.const)
  }

  test("and(true, true, false) == false") {
    test_and(true.const, true.const, false.const)
  }

  test("and(false, false, true) == false") {
    test_and(false.const, false.const, true.const)
  }

  test("and(false, false, false) == false") {
    test_and(false.const, false.const, false.const)
  }

  private def test_||(
    l: Expr.Const[Boolean, OP],
    r: Expr.Const[Boolean, OP],
    m: Expr.Const[Boolean, OP]*,
  )(implicit
    loc: Location,
  ): Unit = {
    val tail = NonEmptyVector(r, m.toVector)
    testLogic(l, tail) {
      _.reduceLeft {
        _ || _
      }
    } { (h, t) =>
      t.foldLeft(h) {
        _ || _
      }
    }
  }

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

  test("true || true || true == true") {
    test_||(true.const, true.const, true.const)
  }

  test("true || true || false == true") {
    test_||(true.const, true.const, false.const)
  }

  test("false || false || true == true") {
    test_||(false.const, false.const, true.const)
  }

  test("false || false || false == false") {
    test_||(false.const, false.const, false.const)
  }

  private def test_or(
    l: Expr.Const[Boolean, OP],
    r: Expr.Const[Boolean, OP],
    m: Expr.Const[Boolean, OP]*,
  )(implicit
    loc: Location,
  ): Unit = {
    val tail = NonEmptyVector(r, m.toVector)
    testLogic(l, tail) {
      _.reduceLeft {
        _ || _
      }
    } { (h, t) =>
      or(h, t.head, t.tail: _*)
    }
  }

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

  test("or(true, true, true) == true") {
    test_or(true.const, true.const, true.const)
  }

  test("or(true, true, false) == true") {
    test_or(true.const, true.const, false.const)
  }

  test("or(false, false, true) == true") {
    test_or(false.const, false.const, true.const)
  }

  test("or(false, false, false) == false") {
    test_or(false.const, false.const, false.const)
  }

  test("!false == true") {
    val expr = !false.const
    val result = expr.run()
    assert(result.state.output)
  }

  test("!true == false") {
    val expr = !true.const
    val result = expr.run()
    assert(!result.state.output)
  }

  test("not(false) == true") {
    val expr = not(false.const)
    val result = expr.run()
    assert(result.state.output)
  }

  test("not(true) == false") {
    val expr = not(true.const)
    val result = expr.run()
    assert(!result.state.output)
  }
}
