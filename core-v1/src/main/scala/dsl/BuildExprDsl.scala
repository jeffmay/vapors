package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import data.FactTypeSet

import cats.Foldable

trait BuildExprDsl {
  self: DslTypes =>

  def apply[II, IO <: OI : OPW, OI >: IO, OO : OPW](
    inputExpr: W[II] ~> W[IO],
    outputExpr: W[OI] ~> W[OO],
  ): Expr.AndThen[W[II], W[IO], W[OI], W[OO], OP]

//  def const[O : OPW](value: W[O]): Expr.Const[W[O], OP]

  def ident[I : OPW]: Expr.Identity[W[I], OP]

  def valuesOfType[T](factTypeSet: FactTypeSet[T])(implicit opTs: OP[Seq[W[T]]]): Expr.ValuesOfType[T, W[T], OP]

  type SpecificHkExprBuilder[I, C[_], E] <: HkExprBuilder[I, C, E]

  implicit def hk[I, C[_], E](expr: W[I] ~> C[W[E]]): SpecificHkExprBuilder[I, C, E]

  trait HkExprBuilder[I, C[_], E] extends Any {

    protected def inputExpr: W[I] ~> C[W[E]]

    def exists(
      conditionExpr: W[E] ~> Boolean,
    )(implicit
      opCE: OP[C[W[E]]],
      opO: OP[Boolean],
      foldC: Foldable[C],
    ): Expr.AndThen[W[I], C[W[E]], C[W[E]], Boolean, OP]
  }
}

final class ValueExprBuilder[V, OP[_]](private val value: V) extends AnyVal {

  def const(implicit op: OP[V]): Expr[Any, V, OP] = Expr.Const(value)
}
