package com.rallyhealth.vapors.v1

import debug.{Debugging, NoDebugging}

import munit.FunSuite

class DebuggingToStringSpec extends FunSuite {

  test("NoDebugging.toString") {
    assertEquals(NoDebugging.toString, "NoDebugging")
  }

  test("Debugging[Any, Int].toString") {
    val debuggingAnyOption = Debugging[Any, Int](_ => {})
    assertEquals(debuggingAnyOption.toString, "Debugging[Any, Int]")
  }

  test("Debugging[Any, Option[Int]].toString") {
    val debuggingAnyOption = Debugging[Any, Option[Int]](_ => {})
    assertEquals(debuggingAnyOption.toString, "Debugging[Any, Option[+Int]]")
  }
}
