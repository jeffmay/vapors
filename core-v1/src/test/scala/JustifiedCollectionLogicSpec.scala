package com.rallyhealth.vapors.v1

class JustifiedCollectionLogicSpec extends munit.FunSuite {

  import dsl.simple.justified._

  test("exists returns true") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 2
    }
    val result = expr.run()
    assert(result.value)
  }

  test("exists returns false") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 3
    }
    val result = expr.run()
    assert(!result.value)
  }
}
