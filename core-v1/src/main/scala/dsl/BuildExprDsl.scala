package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import data.FactTypeSet

import cats.{Foldable, Functor}

trait BuildExprDsl {
  self: DslTypes =>

  def apply[II, IO <: OI : OPW, OI >: IO, OO : OPW](
    inputExpr: W[II] ~> W[IO],
    outputExpr: W[OI] ~> W[OO],
  ): Expr.AndThen[W[II], W[IO], W[OI], W[OO], OP]

//  def const[O : OPW](value: W[O]): Expr.Const[W[O], OP]

  def ident[I : OPW]: Expr.Identity[W[I], OP]

  def valuesOfType[T](factTypeSet: FactTypeSet[T])(implicit opTs: OP[Seq[W[T]]]): Expr.ValuesOfType[T, W[T], OP]

  type SpecificHkExprBuilder[I, C[_], A] <: HkExprBuilder[I, C, A]

  implicit def hk[I, C[_], A](expr: W[I] ~> C[W[A]]): SpecificHkExprBuilder[I, C, A]

  trait HkExprBuilder[I, C[_], A] extends Any {

    protected def inputExpr: W[I] ~> C[W[A]]

    def exists(
      conditionExpr: W[A] ~> Boolean,
    )(implicit
      opCE: OP[C[W[A]]],
      opO: OP[Boolean],
      foldC: Foldable[C],
    ): Expr[W[I], Boolean, OP]

    def map[B](
      mapExpr: W[A] ~> W[B],
    )(implicit
      opA: OP[C[W[A]]],
      opB: OP[C[W[B]]],
      functorC: Functor[C],
    ): W[I] ~> C[W[B]]
  }
}

final class ValueExprBuilder[A, OP[_]](private val value: A) extends AnyVal {

  def const(implicit op: OP[A]): Expr[Any, A, OP] = Expr.Const(value)
}
