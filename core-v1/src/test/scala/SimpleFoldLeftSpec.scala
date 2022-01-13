package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleFoldLeftSpec extends FunSuite {

  import dsl.caching.immutable._

  test(".foldLeft returns initial value when empty") {
    val expr = Seq.empty[Int].const.foldLeft(1.const)(_ + _)
    val observed = expr.run()
    assertEquals(observed, 1)
  }

  test(".foldLeft returns summation of integers") {
    val expr = Seq(1, 2, 3).const.foldLeft(1.const)(_ + _)
    val observed = expr.run()
    val expected = Seq(1, 2, 3).foldLeft(1)(_ + _)
    assertEquals(observed, expected)
  }
}
