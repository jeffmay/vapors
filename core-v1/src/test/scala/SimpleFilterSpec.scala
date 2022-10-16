//package com.rallyhealth.vapors.v1
//
//import cats.data.{NonEmptyList, NonEmptySeq, NonEmptyVector}
//import munit.FunSuite
//
//class SimpleFilterSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  test("Some[Int].filter returns Some[Int]") {
//    val input = Some(1)
//    val expr = input.const.filter(_ < 3.const)
//    assertEquals(expr.run(), input)
//  }
//
//  test("Some[Int].filter returns None") {
//    val input = Some(1)
//    val expr = input.const.filter(_ < 0.const)
//    assertEquals(expr.run(), None)
//  }
//
//  test("None.filter returns None") {
//    val expr = none[Int].filter(_ => true.const)
//    assertEquals(expr.run(), None)
//  }
//
//  test("Seq[Int].filter") {
//    val expr = Seq(1, 2, 3, 4).const.filter(_ < 3.const)
//    val res = expr.run()
//    assertEquals(res, Seq(1, 2))
//  }
//
//  test("Seq[Nothing].filter doesn't compile") {
//    compileErrors {
//      "Seq().const.filter(_ < 3.const)"
//    }
//  }
//
//  test("NonEmptySeq[Int].filter returns a Seq[Int]") {
//    val expr = NonEmptySeq.of(1, 2, 3, 4).const.filter(_ > 2.const)
//    val res = expr.run()
//    assertEquals(res, Seq(3, 4))
//  }
//
//  test("NonEmptySeq[Int].filter returns an empty Seq[Int]") {
//    val expr = NonEmptySeq.of(1, 2, 3, 4).const.filter(_ > 4.const)
//    val res = expr.run()
//    assertEquals(res, Seq())
//  }
//
//  test("NonEmptyList[Int].filter returns a List[Int]") {
//    val expr = NonEmptyList.of(1, 2, 3, 4).const.filter(_ > 2.const)
//    val res = expr.run()
//    assertEquals(res, List(3, 4))
//  }
//
//  test("NonEmptyList[Int].filter returns an empty List[Int]") {
//    val expr = NonEmptyList.of(1, 2, 3, 4).const.filter(_ > 4.const)
//    val res = expr.run()
//    assertEquals(res, Nil)
//  }
//
//  test("NonEmptyVector[Int].filter returns a Vector[Int]") {
//    val expr = NonEmptyVector.of(1, 2, 3, 4).const.filter(_ > 2.const)
//    val res = expr.run()
//    assertEquals(res, Vector(3, 4))
//  }
//
//  test("NonEmptyVector[Int].filter returns an empty Vector[Int]") {
//    val expr = NonEmptyVector.of(1, 2, 3, 4).const.filter(_ > 4.const)
//    val res = expr.run()
//    assertEquals(res, Vector())
//  }
//}
