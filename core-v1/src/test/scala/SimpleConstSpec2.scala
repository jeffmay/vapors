package com.rallyhealth.vapors.v1

import algebra.Expr
import dsl.{NotEmpty, simple2}

import zio.test.*

object SimpleConstSpec2 extends ZIOSpecDefault {

  import dsl.simple2.*

  override val spec: Spec[Any, Any] = suite("dsl.simple.const")(
    test("Option.const allows calling map") {
      val fixture = Option(1)
      val expr: Expr.Const[Option[Int], simple2.OP] = fixture.const
      assertCompletes
//      val expr = fixture.const.map(_ + 1.const)
//      val observed = expr.run()
//      val expected = fixture.map(_ + 1)
//      assertTrue(observed == expected)
    },
  )

//  test("Seq.const allows calling map") {
//    val fixture = Seq(1, 2, 3)
//    val expr = fixture.const.map(_ + 1.const)
//    val observed = expr.run()
//    val expected = fixture.map(_ + 1)
//    assertTrue(observed == expected)
//  }
//
//  test("List.const allows calling map") {
//    val fixture = List(1, 2, 3)
//    val expr = fixture.const.map(_ + 1.const)
//    val observed = expr.run()
//    val expected = fixture.map(_ + 1)
//    assertTrue(observed == expected)
//  }
//
//  test("Nil.const does not allow calling hk") {
//    val message = compileErrors("hk(Nil.const)")
//    assert(message contains NotEmpty.errorMessage.replace("${C}", "[+A]List[A]"))
//  }
//
//  test("Array.const is not allowed") {
//    val fixture = Array(1, 2, 3)
//    val message = compileErrors("fixture.const")
//    assertNotEquals(message, "")
//  }
//
//  test("Iterable.const is not allowed") {
//    val fixture = Iterable(1, 2, 3)
//    val message = compileErrors("fixture.const")
//    assertNotEquals(message, "")
//  }
//
//  test("Set.const does not allow calling map") {
//    val fixture = Set(1, 2, 3)
//    val message = compileErrors("fixture.const.map(_ + 1.const)")
//    assert(message contains "Could not find an instance of Functor for scala.collection.immutable.Set")
//  }
//
//  test("Map.const allows calling map") {
//    val fixture = Map(1 -> 1, 2 -> 2, 3 -> 3)
//    val constExpr = fixture.const
//    val expr = constExpr.map(_ + 1.const)
//    val observed = expr.run()
//    val expected = fixture.map { case (k, v) => (k, v + 1) }
//    assertEquals(observed, expected)
//  }

}
