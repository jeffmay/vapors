package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleContainsAnySpec extends FunSuite {

  import dsl.uncached._

  private val valid = Set(2, 3)

  test("[1, 2].containsAny([2, 3]) == true") {
    val validInput = Seq(1, 2)
    val expr = validInput.const.containsAny(valid.const)
    assert(expr.run())
  }

  test("[1].containsAny([2, 3]) == false") {
    val invalidInput = Seq(1)
    val expr = invalidInput.const.containsAny(valid.const)
    assert(!expr.run())
  }

  test("1 in Set(2, 3) == false") {
    val expr = 1.const in valid.const
    assert(!expr.run())
  }

  test("2 in Set(2, 3) == true") {
    val expr = 2.const in valid.const
    assert(expr.run())
  }

  test("[1, 2].exists(_ in Set(2, 3)) == true") {
    val validInput = Seq(1, 2)
    val expr = validInput.const.exists(_ in valid.const)
    assert(expr.run())
  }

  test("[1].exists(_ in Set(2, 3)) == false") {
    val invalidInput = Seq(1)
    val expr = invalidInput.const.exists(_ in valid.const)
    assert(!expr.run())
  }
}
