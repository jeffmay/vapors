package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import data.FactTypeSet

import cats.Foldable

trait BuildExprDsl extends CommonDsl {

  final def apply[AI, AO <: BI : OP, BI >: AO, BO : OP](
    inputExpr: AI ~> AO,
    outputExpr: BI ~> BO,
  ): Expr.AndThen[AI, AO, BI, BO, OP] = Expr.AndThen(inputExpr, outputExpr)

  final def const[O : OP](value: O): Expr.Const[O, OP] = Expr.Const(value)

  final def ident[I : OP]: Expr.Identity[I, I, OP] = Expr.Identity[I, I, OP]()

  final def valuesOfType[T](factTypeSet: FactTypeSet[T])(implicit opTs: OP[Seq[T]]): Expr.ValuesOfType[T, OP] =
    Expr.ValuesOfType(factTypeSet)

  implicit final def hk[I, C[_], E](expr: I ~> C[E]): HKExprBuilder[I, C, E, OP] = new HKExprBuilder(expr)
}

final class HKExprBuilder[I, C[_], E, P[_]](private val inputExpr: Expr[I, C[E], P]) extends AnyVal with CommonDsl {

  override type OP[a] = P[a]

  def exists(
    conditionExpr: E ~> Boolean,
  )(implicit
    opCE: OP[C[E]],
    opO: OP[Boolean],
    foldC: Foldable[C],
  ): Expr.AndThen[I, C[E], C[E], Boolean, OP] =
    Expr.AndThen(inputExpr, Expr.Exists[C, E, OP](conditionExpr))
}
