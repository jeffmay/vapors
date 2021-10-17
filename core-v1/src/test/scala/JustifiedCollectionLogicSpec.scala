package com.rallyhealth.vapors.v1

class JustifiedCollectionLogicSpec extends munit.FunSuite {

  import dsl.simple.justified._

  test(".exists returns false when empty") {
    val expr = Nil.const.exists {
      ident[Int] > 2
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".exists returns true") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 2
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".exists returns false") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 3
    }
    val result = expr.run()
    assert(!result.value)
  }

  test(".forall returns true when empty") {
    val expr = Nil.const.forall {
      ident[Int] > 1
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".forall returns true when non-empty and condition is met") {
    val expr = List(2, 3).const.exists {
      ident[Int] > 1
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".forall returns false when non-empty and condition is not met") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 1
    }
    val result = expr.run()
    assert(!result.value)
  }
}
