package com.rallyhealth.vapors.v1

package dsl

import algebra.{CombineHolder, Expr, ExprResult}
import data.{ExprState, FactTable, FactTypeSet}
import engine.InterpretExprAsFn

import cats.Foldable

// TODO: Figure out how to mix and match DSLs with different OPs... maybe using Has[_] type?
class VaporsDsl[OP[_]] {

  final type ~>[-I, +O] = Expr[I, O, OP]

  final def apply[AI, AO <: BI : OP, BI >: AO, BO : OP](
    inputExpr: AI ~> AO,
    outputExpr: BI ~> BO,
  ): Expr.AndThen[AI, AO, BI, BO, OP] = Expr.AndThen(inputExpr, outputExpr)

  final def const[O : OP](value: O): Expr.Const[O, OP] = Expr.Const(value)

  final def ident[I : OP]: Expr.Identity[I, I, OP] = Expr.Identity[I, I, OP]()

  final def valuesOfType[T](factTypeSet: FactTypeSet[T])(implicit opTs: OP[Seq[T]]): Expr.ValuesOfType[T, OP] =
    Expr.ValuesOfType(factTypeSet)

  implicit final def run[O](expr: Nothing ~> O): RunExpr[O, OP] = new RunExpr(expr)

  implicit final def runCombine[O : OP](
    builder: CombineHolder[Nothing, Nothing, Any, Nothing, Any, O, OP],
  ): RunExpr[O, OP] =
    new RunExpr(builder.toExpr)

  implicit final def runWith[I, O](expr: I ~> O): RunWithExpr[I, O, OP] = new RunWithExpr(expr)

  implicit final def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): RunWithExpr[I, O, OP] =
    new RunWithExpr(builder.toExpr)

  implicit final def hk[I, C[_], E](expr: I ~> C[E]): HKExprBuilder[I, C, E, OP] = new HKExprBuilder(expr)
}

final class RunExpr[+O, OP[_]](private val expr: Expr[Nothing, O, OP]) extends AnyVal {

  /**
    * Runs the expression without any starting input starting with the given [[FactTable]].
    *
    * @note this requires `()` because this could execute side-effects from the attached debuggers.
    */
  def run(factTable: FactTable = FactTable.empty): ExprResult[Nothing, Nothing, O, OP] = {
    expr.visit(InterpretExprAsFn[OP](ExprState.Empty(factTable)))(implicitly)
  }
}

final class RunWithExpr[-I, +O, OP[_]](private val expr: Expr[I, O, OP]) extends AnyVal {

  /**
    * Runs the expression with the given input and starting [[FactTable]].
    */
  def runWith[In <: I](
    input: In,
    factTable: FactTable = FactTable.empty,
  ): ExprResult[In, I, O, OP] = {
    expr.visit(InterpretExprAsFn[OP](ExprState.Output(input, factTable)))(implicitly)
  }
}

final class HKExprBuilder[I, C[_], E, OP[_]](private val inputExpr: Expr[I, C[E], OP]) extends AnyVal {

  type ~>[-A, +B] = Expr[A, B, OP]

  def exists(
    conditionExpr: E ~> Boolean,
  )(implicit
    opCE: OP[C[E]],
    opO: OP[Boolean],
    foldC: Foldable[C],
  ): Expr.AndThen[I, C[E], C[E], Boolean, OP] =
    Expr.AndThen(inputExpr, Expr.Exists[C, E, OP](conditionExpr))
}
