package com.rallyhealth.vapors.v1

class SimpleExistsFromConstSpec extends munit.FunSuite {

  import dsl.simple._

  test(".exists returns false when empty") {
    val expr = List.empty[Int].const.exists {
      _ > 2.const
    }
    val result = expr.run()
    assert(!result)
  }

  test(".exists returns true when non-empty and condition is met") {
    val expr = List(1, 2, 3).const.exists {
      _ > 2.const
    }
    val result = expr.run()
    assert(result)
  }

  test(".exists returns false when non-empty and condition is not met") {
    val expr = List(1, 2, 3).const.exists {
      _ > 3.const
    }
    val result = expr.run()
    assert(!result)
  }
}
