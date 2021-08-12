package com.rallyhealth

package vapors.v1

import vapors.v1.algebra.Expr._
import vapors.v1.data.FactTable
//import vapors.v1.engine.CatsEngine._

import java.time.{Duration, Instant}

class ArithmeticSpec extends munit.FunSuite {

  import cats.effect.unsafe.implicits.global

  test("1 + 1") {

    val onePlusOne = Const(1) + Const(1)
    // TODO: Move to other tests
    val left = Const(1L) + Const(2)
    val right = Const(1) + Const(2L)
    val x = Const(Instant.now()) + Const(Duration.ofMinutes(5))
    val y = Const(Duration.ofMinutes(5)) + Const(Instant.now())

//    val result = eval(FactTable.empty)(onePlusOne).unsafeRunSync()
//    assert(result == 2, "one plus one is two")
  }
}
