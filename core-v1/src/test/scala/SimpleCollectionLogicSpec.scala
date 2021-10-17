package com.rallyhealth.vapors.v1

class SimpleCollectionLogicSpec extends munit.FunSuite {

  import dsl.simple._

  test("exists returns true") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 2
    }
    val result = expr.run()
    assert(result)
  }

  test("exists returns false") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 3
    }
    val result = expr.run()
    assert(!result)
  }
}
