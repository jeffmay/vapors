package com.rallyhealth.vapors.v1

import munit.FunSuite

class SimpleWhenThenElseSpec extends FunSuite {

  import dsl.simple._

  test("if (true) / else result") {
    val ifResult = 1
    val elseResult = 2
    val expr = when(true.const).thenReturn(ifResult.const).elseReturn(elseResult.const)
    val observed = expr.run()
    assertEquals(observed, ifResult)
  }

  test("if (false) / else result") {
    val ifResult = 1
    val elseResult = 2
    val expr = when(false.const).thenReturn(ifResult.const).elseReturn(elseResult.const)
    val observed = expr.run()
    assertEquals(observed, elseResult)
  }

  test("if (false) / elif (true) / else result") {
    val ifResult = 1
    val elifResult = 2
    val elseResult = 3
    val expr = when(false.const)
      .thenReturn(ifResult.const)
      .elif(true.const)
      .thenReturn(elifResult.const)
      .elseReturn(elseResult.const)
    val observed = expr.run()
    assertEquals(observed, elifResult)
  }

  test("if (false) / elif (false) / else result") {
    val ifResult = 1
    val elifResult = 2
    val elseResult = 3
    val expr = when(false.const)
      .thenReturn(ifResult.const)
      .elif(false.const)
      .thenReturn(elifResult.const)
      .elseReturn(elseResult.const)
    val observed = expr.run()
    assertEquals(observed, elseResult)
  }

  test("if (false) / elif (true) / elif (false) / else result") {
    val ifResult = 1
    val elif1Result = 2
    val elif2Result = 3
    val elseResult = 4
    val expr = when(false.const)
      .thenReturn(ifResult.const)
      .elif(true.const)
      .thenReturn(elif1Result.const)
      .elif(false.const)
      .thenReturn(elif2Result.const)
      .elseReturn(elseResult.const)
    val observed = expr.run()
    assertEquals(observed, elif1Result)
  }

  test("if (false) / elif (false) / elif (true) / else result") {
    val ifResult = 1
    val elif1Result = 2
    val elif2Result = 3
    val elseResult = 4
    val expr = when(false.const)
      .thenReturn(ifResult.const)
      .elif(false.const)
      .thenReturn(elif1Result.const)
      .elif(true.const)
      .thenReturn(elif2Result.const)
      .elseReturn(elseResult.const)
    val observed = expr.run()
    assertEquals(observed, elif2Result)
  }
}
