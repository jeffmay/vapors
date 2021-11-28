package com.rallyhealth.vapors.v1

import algebra.Expr
import data.Justified

import cats.data.NonEmptyList
import munit.{FunSuite, Location}

class SimpleJustifiedBooleanLogicSpec extends FunSuite {

  import dsl.simple.justified._

  private def testLogic(
    l: Any ~:> Justified[Boolean],
    r: Any ~:> Justified[Boolean],
    expected: Justified[Boolean],
  )(
    combine: (Any ~:> Justified[Boolean], Any ~:> Justified[Boolean]) => Any ~:> Justified[Boolean],
  )(implicit
    loc: Location,
  ): Unit = {
    val obtained = combine(l, r).run()
    assertEquals(obtained, expected)
  }

  private def test_&&(
    l: Expr.Const[Justified[Boolean], OP],
    r: Expr.Const[Justified[Boolean], OP],
  )(implicit
    loc: Location,
  ): Unit = {
    val justified = Justified.byInference("and", l.value.value && r.value.value, NonEmptyList.of(l.value, r.value))
    testLogic(l, r, justified)(_ && _)
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

  private def test_and(
    l: Expr.Const[Justified[Boolean], OP],
    r: Expr.Const[Justified[Boolean], OP],
  )(implicit
    loc: Location,
  ): Unit = {
    val justified = Justified.byInference("and", l.value.value && r.value.value, NonEmptyList.of(l.value, r.value))
    testLogic(l, r, justified)(and(_, _))
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

  private def test_||(
    l: Expr.Const[Justified[Boolean], OP],
    r: Expr.Const[Justified[Boolean], OP],
  )(implicit
    loc: Location,
  ): Unit = {
    val justified = Justified.byInference("or", l.value.value || r.value.value, NonEmptyList.of(l.value, r.value))
    testLogic(l, r, justified)(_ || _)
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

  private def test_or(
    l: Expr.Const[Justified[Boolean], OP],
    r: Expr.Const[Justified[Boolean], OP],
  )(implicit
    loc: Location,
  ): Unit = {
    val justified = Justified.byInference("or", l.value.value || r.value.value, NonEmptyList.of(l.value, r.value))
    testLogic(l, r, justified)(or(_, _))
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

  private def testNegation(
    v: Expr.Const[Justified[Boolean], OP],
  )(
    buildExpr: (Any ~:> Justified[Boolean]) => (Any ~:> Justified[Boolean]),
  )(implicit
    loc: Location,
  ): Unit = {
    val expr = buildExpr(v)
    val expected = Justified.byInference("not", !v.value.value, NonEmptyList.of(v.value))
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
