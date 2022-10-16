//package com.rallyhealth.vapors.v1
//
//import cats.data.{NonEmptyList, NonEmptySeq, NonEmptyVector}
//import munit.FunSuite
//
//class SimpleSortedSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  test("Seq[Int].sorted") {
//    val value = Seq(4, 2, 3, 1)
//    val expected = value.sorted
//    val expr = value.const.sorted
//    val result = expr.run()
//    assertEquals(result, expected)
//  }
//
//  test("NonEmptySeq[Int].sorted") {
//    val value = NonEmptySeq.of(4, 2, 3, 1)
//    val expected = value.sorted
//    val expr = value.const.sorted
//    val result = expr.run()
//    assertEquals(result, expected)
//  }
//
//  test("NonEmptyList[Int].sorted") {
//    val value = NonEmptyList.of(4, 2, 3, 1)
//    val expected = value.sorted
//    val expr = value.const.sorted
//    val result = expr.run()
//    assertEquals(result, expected)
//  }
//
//  test("NonEmptyVector[Int].sorted") {
//    val value = NonEmptyVector.of(4, 2, 3, 1)
//    val expected = value.sorted
//    val expr = value.const.sorted
//    val result = expr.run()
//    assertEquals(result, expected)
//  }
//}
