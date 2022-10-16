//package com.rallyhealth.vapors.v1
//
//import data.Justified
//
//import munit.FunSuite
//
//class SimpleJustifiedConcatSpec extends FunSuite {
//
//  import dsl.uncached.justified._
//
//  test("concat non-empty seq with empty seq produces a non-empty seq") {
//    val s1 = Seq(1, 2, 3)
//    val s2 = Seq.empty[Int]
//    val s3 = Seq(4, 5, 6)
//    val expr = concat(s1.const, s2.const, s3.const)
//    val observed = expr.run()
//    val expected =
//      Justified.elements(Justified.byConst(s1)) ++
//        Justified.elements(Justified.byConst(s2)) ++
//        Justified.elements(Justified.byConst(s3))
//    assertEquals(observed, expected)
//  }
//
//  test("concat non-empty option with empty option produces a non-empty seq") {
//    val s1 = Option(1)
//    val s2: Option[Int] = None
//    val s3 = Option(2)
//    val expr = concat(s1.const, s2.const, s3.const)
//    val observed = expr.run()
//    val expected =
//      Justified.elements(Justified.byConst(s1)).toSeq ++
//        Justified.elements(Justified.byConst(s2)) ++
//        Justified.elements(Justified.byConst(s3))
//    assertEquals(observed, expected)
//  }
//}
