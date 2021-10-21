package com.rallyhealth.vapors.v1

import com.rallyhealth.vapors.v1.debug.{Debugging, NoDebugging}
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
    // Unfortunately, this toString is not as nice as it could be if we used more advanced type tags
    // However, requiring type tags for this toString method is probably overkill. If we decide to
    // add more type information to the Debugging class for other purposes, we should also update
    // the toString method and this test.
    assertEquals(debuggingAnyOption.toString, "Debugging[Any, scala.Option]")
  }
}
