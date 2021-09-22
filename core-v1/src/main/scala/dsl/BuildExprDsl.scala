package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import data.FactTypeSet

import cats.{Foldable, Functor}
import com.rallyhealth.vapors.v1.logic.Negation

trait BuildExprDsl {
  self: DslTypes =>

  def apply[II, IO <: OI : OPW, OI >: IO, OO : OPW](
    inputExpr: W[II] ~> W[IO],
    outputExpr: W[OI] ~> W[OO],
  ): Expr.AndThen[W[II], W[IO], W[OI], W[OO], OP]

  def ident[I : OPW]: Expr.Identity[W[I], OP]

  def not[I](expr: W[I] ~> Boolean)(implicit opB: OP[Boolean]): Expr.Not[W[I], OP]

  def valuesOfType[T](factTypeSet: FactTypeSet[T])(implicit opTs: OP[Seq[W[T]]]): Expr.ValuesOfType[T, W[T], OP]

  type SpecificHkExprBuilder[I, C[_], A] <: HkExprBuilder[I, C, A]

  implicit def hk[I, C[_], A](expr: W[I] ~> C[W[A]]): SpecificHkExprBuilder[I, C, A]

  trait HkExprBuilder[I, C[_], A] extends Any {

    protected def inputExpr: W[I] ~> C[W[A]]

    def exists(
      conditionExpr: W[A] ~> Boolean,
    )(implicit
      opA: OP[C[W[A]]],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): Expr[W[I], Boolean, OP]

    def forall(
      conditionExpr: W[A] ~> Boolean,
    )(implicit
      opA: OP[C[W[A]]],
      opB: OP[Boolean],
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

  type SpecificValExprBuilder[I, O] <: ValExprBuilder[I, O]

  implicit def anyVal[I, O](expr: W[I] ~> W[O]): SpecificValExprBuilder[I, O]

  trait ValExprBuilder[I, O] extends Any {

    protected def inputExpr: W[I] ~> W[O]

    def unary_!(
      implicit
      negationO: Negation[W[O]],
      opO: OPW[O],
    ): Expr.AndThen[W[I], W[O], W[O], W[O], OP] = Expr.AndThen(inputExpr, Expr.Not4())
  }

  type SpecificBoolValExprBuilder[I] <: BoolValExprBuilder[I]

  implicit def boolVal[I](expr: W[I] ~> Boolean): SpecificBoolValExprBuilder[I]

  trait BoolValExprBuilder[I] extends Any {

    protected def inputExpr: W[I] ~> Boolean

    def not(implicit opB: OP[Boolean]): Expr.Not[W[I], OP] = Expr.Not(inputExpr)

//    def unary_!(implicit opB: OP[Boolean]): Expr.Not[W[I], OP]
  }
}

final class ConstExprBuilder[A, OP[_]](private val value: A) extends AnyVal {

  def const(implicit op: OP[A]): Expr[Any, A, OP] = Expr.Const(value)
}
