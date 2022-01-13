package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleForAllFromConstSpec extends FunSuite {

  import dsl.caching.immutable._

  test(".forall returns true when empty") {
    val expr = List.empty[Int].const.forall {
      _ > 0.const
    }
    val result = expr.run()
    assert(result)
  }

  test(".forall returns true when non-empty and condition is always met") {
    val expr = List(1, 2, 3).const.forall {
      _ > 0.const
    }
    val result = expr.run()
    assert(result)
  }

  test(".forall returns false when non-empty and condition is not met at least once") {
    val expr = List(1, 2, 3).const.forall {
      _ === 3.const
    }
    val result = expr.run()
    assert(!result)
  }

  test(".forall returns false when non-empty and condition is never met") {
    val expr = List(1, 2, 3).const.forall {
      _ < 0.const
    }
    val result = expr.run()
    assert(!result)
  }
}
