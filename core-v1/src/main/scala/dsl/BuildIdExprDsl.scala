package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import data.FactTypeSet

import cats.Foldable

trait BuildIdExprDsl extends BuildExprDsl with IdExprDsl {

  override final def apply[AI, AO <: BI : OP, BI >: AO, BO : OP](
    inputExpr: AI ~> AO,
    outputExpr: BI ~> BO,
  ): Expr.AndThen[AI, AO, BI, BO, OP] = Expr.AndThen(inputExpr, outputExpr)

//  final def const[O : OP](value: O): Expr.Const[O, OP] = Expr.Const(value)

  override final def ident[I : OP]: Expr.Identity[I, OP] = Expr.Identity[I, OP]()

  override final def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opTs: OP[Seq[T]],
  ): Expr.ValuesOfType[T, T, OP] =
    Expr.ValuesOfType(factTypeSet, _.value)

  implicit def wrap[V](value: V): ValueExprBuilder[V, OP] = new ValueExprBuilder(value)

  override implicit final def hk[I, C[_], E](expr: I ~> C[E]): HkIdExprBuilder[I, C, E] = new HkIdExprBuilder(expr)

  override final type SpecificHkExprBuilder[I, C[_], E] = HkIdExprBuilder[I, C, E]

  final class HkIdExprBuilder[I, C[_], E](override protected val inputExpr: I ~> C[E]) extends HkExprBuilder[I, C, E] {

    override def exists(
      conditionExpr: E ~> Boolean,
    )(implicit
      opCE: OP[C[E]],
      opO: OP[Boolean],
      foldC: Foldable[C],
    ): Expr.AndThen[I, C[E], C[E], Boolean, OP] =
      Expr.AndThen(inputExpr, Expr.Exists[C, E, OP](conditionExpr))
  }
}
