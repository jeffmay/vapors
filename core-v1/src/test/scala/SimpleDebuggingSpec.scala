package com.rallyhealth.vapors.v1

import algebra.Expr

import munit._

class SimpleDebuggingSpec extends FunSuite with CommonDebuggingSpec {

  override final val thisDsl = dsl.simple
  import thisDsl._

  private val initialInput = "Test input"
  private val expectedInitialInput = Some(initialInput)

  private val output = "Test output"
  private val anySeqExpr: Any ~> Seq[String] = output.split(' ').toSeq.const

  test("debug any expr returns the correct output type") {
    anySeqExpr.debug { state =>
      assertEquals(state.output.mkString(" "), output)
    }
  }

  private val constValue = 1
  private val constExpr = 1.const

  test("debug const with input") {
    testExpr(constExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      assertInputEquals(expectedInitialInput, state.input)
      assertEquals(state.output, constValue)
    }
  }

  test("debug const without initial input") {
    testExpr(constExpr).withNoInput.verifyDebuggerCalledWith { state =>
      assertInputEquals(None, state.input)
      assertEquals(state.output, constValue)
    }
  }

  test("debug const syntax works") {
    constExpr.debug { state =>
      assertEquals(state.output, constValue)
    }
  }

  private val combineHolder = 1.const + 3.const + 5.const

  test("debug combine holder with input") {
    val expr = combineHolder.debug { state =>
      val (i, a, b) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(a, 4)
      assertEquals(b, 5)
    }
    expr.runWith(initialInput)
  }

  test("debug combine holder without initial input") {
    val expr = combineHolder.debug { state =>
      val (i, a, b) = state.input
      assertInputEquals(None, i)
      assertEquals(a, 4)
      assertEquals(b, 5)
    }
    run(expr).run()
  }

  private val combineExpr = combineHolder.toExpr

  test("debug combine with input") {
    testExpr(combineExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, a, b) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(a, 4)
      assertEquals(b, 5)
    }
  }

  test("debug combine without initial input") {
    testExpr(combineExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, a, b) = state.input
      assertInputEquals(None, i)
      assertEquals(a, 4)
      assertEquals(b, 5)
    }
  }

  test("debug combine syntax works") {
    combineExpr.debug { state =>
      assertEquals(state.output, 1 + 3 + 5)
    }
  }

  private val customFunctionExpr = Expr.CustomFunction[String, Int, OP]("length", _.length)

  test("debug custom function with input") {
    testExpr(customFunctionExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val i = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(state.output, initialInput.length)
    }
  }

  test("debug custom function syntax works") {
    customFunctionExpr.debug { state =>
      assertEquals(state.output, initialInput.length)
    }
  }

  private val existsInput = Seq(false, true)
  private val existsExpr = existsInput.const.exists(identity)

  test("debug exists with input") {
    testExpr(existsExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(ca, existsInput)
    }
  }

  test("debug exists without initial input") {
    testExpr(existsExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(None, i)
      assertEquals(ca, existsInput)
    }
  }

  test("debug exists syntax works") {
    existsExpr.debug { state =>
      assertEquals(state.output, true)
    }
  }

  private val forAllInput = Seq(false, true)
  private val forAllExpr = forAllInput.const.forall(identity)

  test("debug forall with input") {
    testExpr(forAllExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(ca, forAllInput)
    }
  }

  test("debug forall without initial input") {
    testExpr(forAllExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(None, i)
      assertEquals(ca, forAllInput)
    }
  }

  test("debug forall syntax works") {
    forAllExpr.debug { state =>
      assertEquals(state.output, false)
    }
  }

  private val mapEveryInput = Seq(1, 2)
  private val mapEveryExpr = mapEveryInput.const.map(_ + 1.const)

  test("debug map with input") {
    testExpr(mapEveryExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(ca, mapEveryInput)
    }
  }

  test("debug map without initial input") {
    val seq = Seq(1, 2)
    testExpr(mapEveryExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(None, i)
      assertEquals(ca, seq)
    }
  }

  test("debug map syntax works") {
    mapEveryExpr.debug { state =>
      assertEquals(state.output, mapEveryInput.map(_ + 1))
    }
  }
}
