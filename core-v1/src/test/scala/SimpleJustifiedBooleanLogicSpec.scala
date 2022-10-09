package com.rallyhealth.vapors.v1

import algebra.Expr
import data.Justified
import cats.data.{NonEmptySeq, NonEmptyVector}
import munit.{FunSuite, Location}

class SimpleJustifiedBooleanLogicSpec extends FunSuite {

  import dsl.uncached.justified._

  private def testLogic(
    head: Expr.Const[Justified[Boolean], OP],
    tail: NonEmptyVector[Expr.Const[Justified[Boolean], OP]],
  )(
    expect: NonEmptyVector[Justified[Boolean]] => Justified[Boolean],
  )(
    combine: (Any ~:> Justified[Boolean], NonEmptyVector[Any ~:> Justified[Boolean]]) => Any ~:> Justified[Boolean],
  )(implicit
    loc: Location,
  ): Unit = {
    val obtained = combine(head, tail).run()
    val expected = expect((head +: tail).map(_.value))
    assertEquals(obtained, expected)
  }

  private def test_&&(
    l: Expr.Const[Justified[Boolean], OP],
    r: Expr.Const[Justified[Boolean], OP],
    m: Expr.Const[Justified[Boolean], OP]*,
  )(implicit
    loc: Location,
  ): Unit = {
    val tail = NonEmptyVector(r, m.toVector)
    testLogic(l, tail) {
      _.reduceLeft { (l, r) =>
        Justified.byInference("and", l.value && r.value, NonEmptySeq.of(l, r))
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
    l: Expr.Const[Justified[Boolean], OP],
    r: Expr.Const[Justified[Boolean], OP],
    m: Expr.Const[Justified[Boolean], OP]*,
  )(implicit
    loc: Location,
  ): Unit = {
    val tail = NonEmptyVector(r, m.toVector)
    testLogic(l, tail) {
      _.reduceLeft { (l, r) =>
        Justified.byInference("and", l.value && r.value, NonEmptySeq.of(l, r))
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
    l: Expr.Const[Justified[Boolean], OP],
    r: Expr.Const[Justified[Boolean], OP],
    m: Expr.Const[Justified[Boolean], OP]*,
  )(implicit
    loc: Location,
  ): Unit = {
    val tail = NonEmptyVector(r, m.toVector)
    testLogic(l, tail) {
      _.reduceLeft { (l, r) =>
        Justified.byInference("or", l.value || r.value, NonEmptySeq.of(l, r))
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
    l: Expr.Const[Justified[Boolean], OP],
    r: Expr.Const[Justified[Boolean], OP],
    m: Expr.Const[Justified[Boolean], OP]*,
  )(implicit
    loc: Location,
  ): Unit = {
    val tail = NonEmptyVector(r, m.toVector)
    testLogic(l, tail) {
      _.reduceLeft { (l, r) =>
        Justified.byInference("or", l.value || r.value, NonEmptySeq.of(l, r))
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

  private def testNegation(
    v: Expr.Const[Justified[Boolean], OP],
  )(
    buildExpr: (Any ~:> Justified[Boolean]) => (Any ~:> Justified[Boolean]),
  )(implicit
    loc: Location,
  ): Unit = {
    val expr = buildExpr(v)
    val expected = Justified.byInference("not", !v.value.value, NonEmptySeq.of(v.value))
    val obtained = expr.run()
    assertEquals(obtained, expected)
  }

  private def test_!(v: Expr.Const[Justified[Boolean], OP])(implicit loc: Location): Unit =
    testNegation(v)(!_)

  test("!true == false") {
    test_!(true.const)
  }

  test("!false == true") {
    test_!(false.const)
  }

  private def test_not(v: Expr.Const[Justified[Boolean], OP])(implicit loc: Location): Unit =
    testNegation(v)(not(_))

  test("not(true) == false") {
    test_not(true.const)
  }

  test("not(false) == true") {
    test_not(false.const)
  }
}
