package com.rallyhealth.vapors.v1

import algebra.Expr
import com.rallyhealth.vapors.v1.example.NestedSelectable
import com.rallyhealth.vapors.v1.lens.VariantLens
import munit._
import shapeless.HNil

class SimpleDebuggingSpec extends FunSuite with CommonDebuggingSpec {

  override final val thisDsl = dsl.simple
  import thisDsl._

  private val initialInput = "Test input"
  private val expectedInitialInput = Some(initialInput)

  private val anySeqExpr: Any ~:> Seq[String] = Seq("generic", "expression").const

  test("debug any expr syntax produces the correct output type") {
    anySeqExpr.debug { state =>
      val o: Seq[String] = state.output
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

  test("debug const syntax produces the correct types") {
    constExpr.debug { state =>
      val o: Int = state.output
    }
  }

  private val combineHolder = 1.const + 3.const + 5.const
  private val combineHolderOutput = 1 + 3 + 5

  test("debug combine holder with input") {
    val expr = combineHolder.debug { state =>
      val (i, a, b) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(a, 4)
      assertEquals(b, 5)
      assertEquals(state.output, combineHolderOutput)
    }
    expr.runWith(initialInput)
  }

  test("debug combine holder without initial input") {
    val expr = combineHolder.debug { state =>
      val (i, a, b) = state.input
      assertInputEquals(None, i)
      assertEquals(a, 4)
      assertEquals(b, 5)
      assertEquals(state.output, combineHolderOutput)
    }
    expr.run()
  }

  private val combineExpr = combineHolder.toExpr

  test("debug combine with input") {
    testExpr(combineExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, a, b) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(a, 4)
      assertEquals(b, 5)
      assertEquals(state.output, combineHolderOutput)
    }
  }

  test("debug combine without initial input") {
    testExpr(combineExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, a, b) = state.input
      assertInputEquals(None, i)
      assertEquals(a, 4)
      assertEquals(b, 5)
      assertEquals(state.output, combineHolderOutput)
    }
  }

  test("debug combine syntax works") {
    combineExpr.debug { state =>
      val (_, l: Int, r: Int) = state.input
      val o: Int = state.output
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
      val o: Int = state.output
    }
  }

  private val existsInput = Seq(false, true)
  private val existsExpr = existsInput.const.exists(identity)
  private val existsExprOutput = existsInput.exists(identity)

  test("debug exists with input") {
    testExpr(existsExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(ca, existsInput)
      assertEquals(state.output, existsExprOutput)
    }
  }

  test("debug exists without initial input") {
    testExpr(existsExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(None, i)
      assertEquals(ca, existsInput)
      assertEquals(state.output, existsExprOutput)
    }
  }

  test("debug exists syntax produces the correct types") {
    existsExpr.debug { state =>
      val (_, ca: Seq[Boolean]) = state.input
      val o: Boolean = state.output
    }
  }

  private val forAllInput = Seq(false, true)
  private val forAllExpr = forAllInput.const.forall(identity)
  private val forAllExprOutput = forAllInput.forall(identity)

  test("debug forall with input") {
    testExpr(forAllExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(ca, forAllInput)
      assertEquals(state.output, forAllExprOutput)
    }
  }

  test("debug forall without initial input") {
    testExpr(forAllExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(None, i)
      assertEquals(ca, forAllInput)
      assertEquals(state.output, forAllExprOutput)
    }
  }

  test("debug forall syntax produces the correct types") {
    forAllExpr.debug { state =>
      val (_, ca: Seq[Boolean]) = state.input
      val o: Boolean = state.output
    }
  }

  private val mapEveryInput = Seq(1, 2)
  private val mapEveryExpr = mapEveryInput.const.map(_ + 1.const)
  private val mapEveryExprOutput = mapEveryInput.map(_ + 1)

  test("debug map with input") {
    testExpr(mapEveryExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(ca, mapEveryInput)
      assertEquals(state.output, mapEveryExprOutput)
    }
  }

  test("debug map without initial input") {
    testExpr(mapEveryExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, ca) = state.input
      assertInputEquals(None, i)
      assertEquals(ca, mapEveryInput)
      assertEquals(state.output, mapEveryExprOutput)
    }
  }

  test("debug map syntax produces the correct types") {
    mapEveryExpr.debug { state =>
      val (_, ca: Seq[Int]) = state.input
      val o: Seq[Int] = state.output
    }
  }

  private val selectInput = NestedSelectable("optSelected", opt = Some(NestedSelectable("selected")))
  private val selectExpr = selectInput.const.get(_.select(_.opt))
  private val selectExprLensPath = VariantLens.id[NestedSelectable].select(_.opt).path
  private val selectExprOutput = selectInput.opt

  test("debug select with input") {
    testExpr(selectExpr).withInput(initialInput).verifyDebuggerCalledWith { state =>
      val (i, a, lens, b) = state.input
      assertInputEquals(expectedInitialInput, i)
      assertEquals(lens.path, selectExprLensPath)
      assertEquals(a, selectInput)
      assertEquals(b, selectInput.opt)
      assertEquals(state.output, selectExprOutput)
    }
  }

  test("debug select without initial input") {
    testExpr(selectExpr).withNoInput.verifyDebuggerCalledWith { state =>
      val (i, a, lens, b) = state.input
      assertInputEquals(None, i)
      assertEquals(lens.path, selectExprLensPath)
      assertEquals(a, selectInput)
      assertEquals(b, selectInput.opt)
      assertEquals(state.output, selectExprOutput)
    }
  }

  test("debug map syntax produces the correct types") {
    selectExpr.debug { state =>
      val (
        _,
        a: NestedSelectable,
        lens: VariantLens[NestedSelectable, Option[NestedSelectable]],
        b: Option[NestedSelectable],
      ) = state.input
      val o: Option[NestedSelectable] = state.output
    }
  }

  private val zipToHListExpr0 = ("a".const :: 1.const).toHList
  private val zipToHListOutput0 = "a" :: 1 :: HNil

  test("debug zipToHList without initial input") {
    testExpr(zipToHListExpr0).withNoInput.verifyDebuggerCalledWith { state =>
      val i = state.input
      assertInputEquals(None, i)
      assertEquals(state.output, zipToHListOutput0)
    }
  }

  test("debug zipToHList syntax works") {
    zipToHListExpr0.debug { state =>
      assertEquals(state.output, zipToHListOutput0)
    }
  }

  private val zipToHListInput1 = 1
  private val zipToHListExpr1 = (ident[Int] :: "bananas".const).toHList
  private val zipToHListOutput1 = zipToHListInput1 :: "bananas" :: HNil

  test("debug zipToHList with input") {
    testExpr(zipToHListExpr1).withInput(zipToHListInput1).verifyDebuggerCalledWith { state =>
      val i = state.input
      assertInputEquals(Some(zipToHListInput1), i)
      assertEquals(state.output, zipToHListOutput1)
    }
  }

  test("debug zipToHList with input syntax works") {
    zipToHListExpr1.debug { state =>
      assertEquals(state.input, zipToHListInput1)
      assertEquals(state.output, zipToHListOutput1)
    }
  }
}
