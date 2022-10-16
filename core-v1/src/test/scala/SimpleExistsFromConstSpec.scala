//package com.rallyhealth.vapors.v1
//
//import munit.FunSuite
//
//class SimpleExistsFromConstSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  test(".exists returns false when empty") {
//    val expr = List.empty[Int].const.exists {
//      _ > 0.const
//    }
//    val result = expr.run()
//    assert(!result)
//  }
//
//  test(".exists returns true when non-empty and condition is always met") {
//    val expr = List(1, 2, 3).const.exists {
//      _ > 0.const
//    }
//    val result = expr.run()
//    assert(result)
//  }
//
//  test(".exists returns true when non-empty and condition is met at least once") {
//    val expr = List(1, 2, 3).const.exists {
//      _ === 3.const
//    }
//    val result = expr.run()
//    assert(result)
//  }
//
//  test(".exists returns false when non-empty and condition is never met") {
//    val expr = List(1, 2, 3).const.exists {
//      _ < 0.const
//    }
//    val result = expr.run()
//    assert(!result)
//  }
//}
