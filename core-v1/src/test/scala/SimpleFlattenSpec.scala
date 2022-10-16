//package com.rallyhealth.vapors.v1
//
//import munit.FunSuite
//
//class SimpleFlattenSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  def alphaAt[C[_]](n: Int): Char = ('A' + n).toChar
//
//  test("List[List[Char]].flatten infers a List") {
//    val values = List(1, 2, 3).map(i => List.fill(i)(alphaAt(i)))
//    val expr: Any ~:> List[Char] = values.const.flatten
//    val expected = values.flatten
//    val observed = expr.run()
//    assertEquals(observed, expected)
//  }
//
//  test("Seq[List[Char]].flatten infers a Seq") {
//    val values = Seq(1, 2, 3).map(i => List.fill(i)(alphaAt(i)))
//    val expr: Any ~:> Seq[Char] = values.const.flatten[Seq]
//    val expected = values.flatten
//    val observed = expr.run()
//    assertEquals(observed, expected)
//  }
//
//  test("List[Seq[Char]].flatten[Seq] compiles when explicit") {
//    val values = List(1, 2, 3).map(i => Seq.fill(i)(alphaAt(i)))
//    val expr: Any ~:> Seq[Char] = values.const.flatten[Seq]
//    val expected = values.flatten
//    val observed = expr.run()
//    assertEquals(observed, expected)
//  }
//
//  test("Vector[List[Char]].flatten[Seq] compiles when explicit") {
//    val values = Vector(1, 2, 3).map(i => List.fill(i)(alphaAt(i)))
//    val expr: Any ~:> Seq[Char] = values.const.flatten[Seq]
//    val expected = values.flatten
//    val observed = expr.run()
//    assertEquals(observed, expected)
//  }
//
//  test("Vector[Int].flatMap(Seq[Int])") {
//    val values = Vector(1, 2, 3)
//    val innerValues = Seq(2, 3, 4)
//    val expr = values.const.flatMap { _ =>
//      innerValues.const
//    }
//    val observed = expr.run()
//    val expected = values.flatMap(_ => innerValues)
//    assertEquals(observed, expected)
//  }
//
//  test("Seq[Int].flatMap(Vector[Int])") {
//    val values = Seq(1, 2, 3)
//    val innerValues = Vector(2, 3, 4)
//    val expr = values.const.flatMap { _ =>
//      innerValues.const
//    }
//    val observed = expr.run()
//    val expected = values.flatMap(_ => innerValues)
//    assertEquals(observed, expected)
//  }
//
//  test("List[Int].flatMap[Seq, Int](Vector[Int]) compiles when explicit") {
//    val values = List(1, 2, 3)
//    val innerValues = Vector(2, 3, 4)
//    val expr = values.const.flatMap[Seq, Int] { _ =>
//      innerValues.const
//    }
//    val observed = expr.run()
//    val expected = values.flatMap(_ => innerValues)
//    assertEquals(observed, expected)
//  }
//}
