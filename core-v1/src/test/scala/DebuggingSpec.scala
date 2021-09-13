package com.rallyhealth.vapors.v1

import data.{ExprState, FactTable}
import debug.{Debugging, NoDebugging}
import dsl.simple._

import munit._

class DebuggingSpec extends FunSuite {

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

  def runTestsWith[I](expectInput: Option[I]): Unit = {
    val withOrWithout = if (expectInput.isDefined) "with" else "without"
    def validateInput(debugInput: Any)(implicit loc: Location): Unit = expectInput match {
      case Some(expectedValue) => assertEquals(debugInput, expectedValue)
      case None => assertEquals(debugInput, ExprState.Nothing)
    }
    def testExpr(
      expr: I ~> Any,
      factTable: FactTable = FactTable.empty,
    ): Unit = expectInput match {
      case Some(expectedValue) => expr.runWith(expectedValue, factTable)
      case None => expr.run(factTable)
    }

    test(s"debug const $withOrWithout input") {
      testExpr {
        const(1)
          .debug { state =>
            validateInput(state.input)
            assertEquals(state.output, 1)
          }
      }
    }

    test(s"debug combine holder $withOrWithout input") {
      testExpr {
        (const(1) + const(3) + const(5))
          .debug { state =>
            val (i, a, b) = state.input
            validateInput(i)
            assertEquals(a, 4)
            assertEquals(b, 5)
          }
      }
    }

    test(s"debug exists $withOrWithout input") {
      val seq = Seq(false, true)
      testExpr {
        const(seq)
          .exists(ident)
          .debug { state =>
            val (i, ce) = state.input
            validateInput(i)
            assertEquals(ce, seq)
          }
      }
    }
  }

  runTestsWith(Some("Test Input"))
  runTestsWith(None)
}
