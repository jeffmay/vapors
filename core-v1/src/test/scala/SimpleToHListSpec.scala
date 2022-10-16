//package com.rallyhealth.vapors.v1
//
//import example.Person
//import munit.FunSuite
//
//class SimpleToHListSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  test("(String :: Int).toHList.as[Person]") {
//    val expected = Person("Alice", 40)
//    val xhl = expected.name.const :: expected.age.const
//    val xp: Any ~:> Person = xhl.toHList.as[Person]
//    val observed = xp.run()
//    assertEquals(observed, expected)
//  }
//
//  test("wrap(String, Int).as[Person]") {
//    val expected = Person("Alice", 40)
//    val xhl = wrap(expected.name.const, expected.age.const)
//    val xp: Any ~:> Person = xhl.toHList.as[Person]
//    val observed = xp.run()
//    assertEquals(observed, expected)
//  }
//
//  test("wrap(Seq[String], String[Int]).zipToShortest") {
//    val expected = Seq(Person("Alice", 40), Person("Bob", 45))
//    val names = expected.map(_.name)
//    val ages = expected.map(_.age)
//    val xhl = wrap(names.const, ages.const).zipToShortest
//    val expr = xhl.map { hl =>
//      hl.as[Person]
//    }
//    val observed = expr.run()
//    assertEquals(observed, expected)
//  }
//}
