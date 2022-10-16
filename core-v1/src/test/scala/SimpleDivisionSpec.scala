//package com.rallyhealth.vapors.v1
//
//import munit.FunSuite
//
//class SimpleDivisionSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  // TODO: Should this return an Option[Int]? Maybe another method for this?
//  test("Divide by zero") {
//    val expr = 1.const / 0.const
//    val exc = intercept[ArithmeticException] {
//      expr.run()
//    }
//    assert(exc.getMessage contains "zero")
//  }
//
//  test("Int / Int (exact)") {
//    val expr = 2.const / 1.const
//    val observed = expr.run()
//    assertEquals(observed, 2)
//  }
//
//  test("Int / Int (floor)") {
//    val expr = 1.const / 2.const
//    val observed = expr.run()
//    assertEquals(observed, 0)
//  }
//
//  test("Int / Int / Int (exact)") {
//    val expr = 8.const / 2.const / 2.const
//    val observed = expr.run()
//    assertEquals(observed, 2)
//  }
//
//  test("Long / Int (exact)") {
//    val expr = 4L.const / 2.const
//    val observed = expr.run()
//    assertEquals(observed, 2L)
//  }
//
//  test("Long / Int (floor)") {
//    val expr = 2L.const / 4.const
//    val observed = expr.run()
//    assertEquals(observed, 0L)
//  }
//
//  test("Long / Int / Int (exact)") {
//    val expr = 8L.const / 2.const / 2.const
//    val observed = expr.run()
//    assertEquals(observed, 2L)
//  }
//
//  test("Int / Long (exact)") {
//    val expr = 4.const / 2L.const
//    val observed = expr.run()
//    assertEquals(observed, 2L)
//  }
//
//  test("Int / Long (floor)") {
//    val expr = 2.const / 4L.const
//    val observed = expr.run()
//    assertEquals(observed, 0L)
//  }
//
//  test("Int / Long / Int (exact)") {
//    val expr = 8.const / 2L.const / 2.const
//    val observed = expr.run()
//    assertEquals(observed, 2L)
//  }
//
//  test("Int / Int / Long (exact)") {
//    val expr = 8.const / 2.const / 2L.const
//    val observed = expr.run()
//    assertEquals(observed, 2L)
//  }
//
//  test("Double / Int") {
//    val expr = 2d.const / 4.const
//    val observed = expr.run()
//    assertEquals(observed, 0.5d)
//  }
//
//  test("Double / Int / Long") {
//    val expr = 4d.const / 4.const / 2L.const
//    val observed = expr.run()
//    assertEquals(observed, 0.5d)
//  }
//
//  test("Int / Double") {
//    val expr = 2.const / 4d.const
//    val observed = expr.run()
//    assertEquals(observed, 0.5d)
//  }
//
//  test("Int / Double / Long") {
//    val expr = 4.const / 4d.const / 2L.const
//    val observed = expr.run()
//    assertEquals(observed, 0.5d)
//  }
//
//  test("Float / Int") {
//    val expr = 2f.const / 4.const
//    val observed = expr.run()
//    assertEquals(observed, 0.5f)
//  }
//
//  test("Float / Int / Long") {
//    val expr = 4f.const / 4.const / 2L.const
//    val observed = expr.run()
//    assertEquals(observed, 0.5f)
//  }
//
//  test("Int / Float") {
//    val expr = 2.const / 4f.const
//    val observed = expr.run()
//    assertEquals(observed, 0.5f)
//  }
//
//  test("Int / Float / Long") {
//    val expr = 4.const / 4f.const / 2L.const
//    val observed = expr.run()
//    assertEquals(observed, 0.5f)
//  }
//}
