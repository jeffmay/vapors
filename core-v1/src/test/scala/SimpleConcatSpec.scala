package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleConcatSpec extends FunSuite {

  import dsl.simple._

  test("concat non-empty seq with empty seq produces a non-empty seq") {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq.empty[Int]
    val s3 = Seq(4, 5, 6)
    val expr = concat(s1.const, s2.const, s3.const)
    val observed = expr.run()
    val expected = s1 ++ s2 ++ s3
    assertEquals(observed, expected)
  }
}
