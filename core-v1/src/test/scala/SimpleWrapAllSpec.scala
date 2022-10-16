//package com.rallyhealth.vapors.v1
//
//import cats.data.NonEmptySeq
//import munit.FunSuite
//
//class SimpleWrapAllSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  test("wrapAll(None)") {
//    val expr = wrapAll(None: Option[Any ~:> Int])
//    val observed = expr.run()
//    assertEquals(observed, None)
//  }
//
//  test("wrapAll(Some(expr)) with input") {
//    val expr = wrapAll(Option((ident[Int] + 1.const).toExpr))
//    val observed = expr.runWith(1)
//    assertEquals(observed, Some(2))
//  }
//
//  test("wrapAll List of mixed input expressions") {
//    val expr = wrapAll(List(1.const, (ident[Int] + 1.const).toExpr, (ident[Int] * 2.const).toExpr)).map(_ * 2.const)
//    val input = 2
//    val expected = List(1, input + 1, input * 2).map(_ * 2)
//    val observed = expr.runWith(input)
//    assertEquals(observed, expected)
//  }
//
//  test("wrapAll NonEmptySeq of mixed input expressions") {
//    val expr =
//      wrapAll(NonEmptySeq.of(1.const, (ident[Int] + 1.const).toExpr, (ident[Int] * 2.const).toExpr)).map(_ * 2.const)
//    val input = 2
//    val expected = NonEmptySeq.of(1, input + 1, input * 2).map(_ * 2)
//    val observed = expr.runWith(input)
//    assertEquals(observed, expected)
//  }
//}
