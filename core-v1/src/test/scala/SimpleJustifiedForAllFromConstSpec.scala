package com.rallyhealth.vapors.v1

class SimpleJustifiedForAllFromConstSpec extends munit.FunSuite {

  import dsl.simple.justified._

  test(".forall returns true when empty") {
    val expr = List.empty[Int].const.forall {
      _ > 1
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".forall returns true when non-empty and condition is met") {
    val expr = List(2, 3).const.exists {
      _ > 1
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".forall returns false when non-empty and condition is not met") {
    val expr = List(1, 2, 3).const.forall {
      _ > 1
    }
    val result = expr.run()
    assert(!result.value)
  }
}
