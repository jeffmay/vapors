package com.rallyhealth.vapors.v1

class SimpleJustifiedExistsFromConstSpec extends munit.FunSuite {

  import dsl.simple.justified._

  test(".exists returns false when empty") {
    val expr = List.empty[Int].const.exists {
      _ > 2
    }
    val result = expr.run()
    assert(!result.value)
  }

  test(".exists returns true") {
    val expr = List(1, 2, 3).const.exists {
      _ > 2
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".exists returns false") {
    val expr = List(1, 2, 3).const.exists {
      _ > 3
    }
    val result = expr.run()
    assert(!result.value)
  }
}
