package com.rallyhealth.vapors.v1

class SimpleCollectionLogicSpec extends munit.FunSuite {

  import dsl.simple._

  test(".exists returns false when empty") {
    val expr = Nil.const.exists {
      ident[Int] > 2
    }
    val result = expr.run()
    assert(result)
  }

  test(".exists returns true when non-empty and condition is met") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 2
    }
    val result = expr.run()
    assert(result)
  }

  test(".exists returns false when non-empty and condition is not met") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 3
    }
    val result = expr.run()
    assert(!result)
  }

  test(".forall returns true when empty") {
    val expr = Nil.const.forall {
      ident[Int] > 1
    }
    val result = expr.run()
    assert(result)
  }

  test(".forall returns true when non-empty and condition is met") {
    val expr = List(2, 3).const.exists {
      ident[Int] > 1
    }
    val result = expr.run()
    assert(result)
  }

  test(".forall returns false when non-empty and condition is not met") {
    val expr = List(1, 2, 3).const.exists {
      ident[Int] > 1
    }
    val result = expr.run()
    assert(!result)
  }
}
