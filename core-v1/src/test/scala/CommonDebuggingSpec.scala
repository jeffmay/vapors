package com.rallyhealth.vapors.v1

import data.{ExprState, FactTable}
import debug.DebugArgs

import izumi.reflect.Tag
import munit.Assertions._
import munit.Location

import scala.reflect.ClassTag

trait CommonDebuggingSpec extends BaseDslSpec {

  import thisDsl._

  protected def assertInputEquals[I](
    expected: Option[I],
    observed: Any,
  )(implicit
    loc: Location,
  ): Unit = expected match {
    case Some(expectation) => assertEquals(observed, expectation)
    case None => assertEquals(observed, ExprState.Nothing)
  }

  def testExpr[E <: AnyExpr](
    expr: E,
  )(implicit
    debugArgs: DebugArgs[E, OP],
  ): TestDebugExprWith[E, debugArgs.In, debugArgs.Out] =
    new TestDebugExprWith[E, debugArgs.In, debugArgs.Out](expr)(debugArgs)

  final class TestDebugExprWith[E <: AnyExpr, DI, DO](expr: E)(implicit debugArgs: DebugArgs.Aux[E, OP, DI, DO]) {

    def withInput[I, S >: E <: I ~:> Any](
      input: I,
    )(implicit
      ev: E <:< S,
    ): TestInputDebugExpr[S, I, DI, DO] = {
      new TestInputDebugExpr[S, I, DI, DO](expr, input, FactTable.empty)(
        debugArgs.asInstanceOf[DebugArgs.Aux[S, OP, DI, DO]],
      )
    }

    def withNoInput[S >: E <: Any ~:> Any](implicit ev: E <:< S): TestAnyDebugExpr[S, DI, DO] = {
      new TestAnyDebugExpr[S, DI, DO](expr, FactTable.empty)(
        debugArgs.asInstanceOf[DebugArgs.Aux[S, OP, DI, DO]],
      )
    }
  }

  sealed trait TestDebugExpr[E <: AnyExpr, DI, DO] {

    protected def expr: E
    protected def debugArgs: DebugArgs.Aux[E, OP, DI, DO]

    protected def runDebuggedExpr(debuggedExpr: E): Unit

    def verifyDebuggerCalledWith(
      test: ExprState[DI, DO] => Unit,
    )(implicit
      cti: ClassTag[DI],
      tti: Tag[DI],
      cto: ClassTag[DO],
      tto: Tag[DO],
      cte: ClassTag[E],
      tte: Tag[E],
    ): Unit = {
      var debugInvoked = false
      val debuggedExpr = DebugArgs[OP].of(expr)(debugArgs).debug { state =>
        debugInvoked = true
        test(state)
      }
      runDebuggedExpr(debuggedExpr)
      assert(debugInvoked, s"debugger never invoked by the Expr.Visitor interpreter invoked by dsl.$thisDsl")
    }
  }

  final class TestAnyDebugExpr[E <: Any ~:> Any, DI, DO](
    override protected val expr: E,
    factTable: FactTable,
  )(implicit
    override protected val debugArgs: DebugArgs.Aux[E, OP, DI, DO],
  ) extends TestDebugExpr[E, DI, DO] {

    def withFactTable(factTable: FactTable): TestAnyDebugExpr[E, DI, DO] =
      new TestAnyDebugExpr(expr, factTable)

    override protected def runDebuggedExpr(debuggedExpr: E): Unit = {
      debuggedExpr.run(factTable)
    }
  }

  final class TestInputDebugExpr[E <: I ~:> Any, I, DI, DO](
    override protected val expr: E,
    expectInput: I,
    factTable: FactTable,
  )(implicit
    override protected val debugArgs: DebugArgs.Aux[E, OP, DI, DO],
  ) extends TestDebugExpr[E, DI, DO] {

    def withFactTable(factTable: FactTable): TestInputDebugExpr[E, I, DI, DO] =
      new TestInputDebugExpr(expr, expectInput, factTable)

    override protected def runDebuggedExpr(debuggedExpr: E): Unit = {
      debuggedExpr.runWith(expectInput, factTable)
    }
  }
}
