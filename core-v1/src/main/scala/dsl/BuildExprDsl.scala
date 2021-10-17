package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.{FactTypeSet, Window}
import logic.Negation

import cats.{Foldable, Functor, Order}

import scala.annotation.nowarn
import scala.collection.Factory

trait BuildExprDsl {
  self: DslTypes =>

  protected implicit def compareWrapped: CompareWrapped[W]

  protected implicit def windowComparable: WindowComparable[W, OP]

  protected implicit def extract: Extract[W]

  protected implicit def wrapConst: WrapConst[W]

  def apply[II, IO <: OI : OPW, OI >: IO, OO : OPW](
    inputExpr: W[II] ~> W[IO],
    outputExpr: W[OI] ~> W[OO],
  ): Expr.AndThen[W[II], W[IO], W[OI], W[OO], OP]

  def ident[I : OPW]: Expr.Identity[W[I], OP]

  def not[I, O : OPW](expr: W[I] ~> W[O])(implicit negation: Negation[W[O]]): Expr.Not[W[I], W[O], OP]

  def valuesOfType[T](factTypeSet: FactTypeSet[T])(implicit opTs: OP[Seq[W[T]]]): Expr.ValuesOfType[T, W[T], OP]

  implicit def wrap[A](value: A)(implicit constType: WrapConstType[W, A]): ConstExprBuilder[constType.Out, OP] =
    new ConstExprBuilder(constType(wrapConst.wrapConst(value)))

  type SpecificHkExprBuilder[I, C[_], A] <: HkExprBuilder[I, C, A]

  implicit def hk[I, C[_], A](expr: W[I] ~> C[W[A]]): SpecificHkExprBuilder[I, C, A]

  trait HkExprBuilder[I, C[_], A] extends Any {

    protected def inputExpr: W[I] ~> C[W[A]]

    def exists(
      conditionExpr: W[A] ~> W[Boolean],
    )(implicit
      opA: OP[C[W[A]]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): Expr[W[I], W[Boolean], OP]

    def forall(
      conditionExpr: W[A] ~> W[Boolean],
    )(implicit
      opA: OP[C[W[A]]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): Expr[W[I], W[Boolean], OP]

    def map[B](
      mapExpr: W[A] ~> W[B],
    )(implicit
      opA: OP[C[W[A]]],
      opB: OP[C[W[B]]],
      functorC: Functor[C],
    ): W[I] ~> C[W[B]]
  }

  implicit def compare[I, V : Order : OP](
    valueExpr: I ~> W[V],
  )(implicit
    opV: OP[W[V]],
    opW: OP[W[Window[V]]],
    opB: OP[W[Boolean]],
  ): ComparisonExprBuilder[I, V] = new ComparisonExprBuilder(valueExpr)

  class ComparisonExprBuilder[I, V : Order : OP](
    protected val valueExpr: I ~> W[V],
  )(implicit
    opV: OP[W[V]],
    opW: OP[W[Window[V]]],
    opB: OP[W[Boolean]],
  ) {

    // TODO: Use some kind of function metadata object instead of a separate name parameter
    private def compareExpr(
      name: String,
      that: Expr[I, W[V], OP],
    )(
      using: V => Window[V],
    ): Expr.WithinWindow[I, V, W, OP] = {
      Expr.WithinWindow(
        valueExpr,
        Expr.AndThen(
          that,
          Expr.CustomFunction[W[V], W[Window[V]], OP](name, CompareWrapped[W].map(_)(using)),
        ),
      )
    }

    protected def compareLiteral(
      @nowarn name: String, // this is unused but kept for consistency
      that: V,
    )(
      using: V => Window[V],
    ): Expr.WithinWindow[I, V, W, OP] =
      Expr.WithinWindow(
        valueExpr,
        Expr.Const[W[Window[V]], OP](CompareWrapped[W].wrapConst(using(that))),
      )

    def <(literal: V): I ~> W[Boolean] = compareLiteral("<", literal)(Window.lessThan(_))

    def <(expr: I ~> W[V]): I ~> W[Boolean] = compareExpr("<", expr)(Window.lessThan(_))

    def <=(literal: V): I ~> W[Boolean] = compareLiteral("<=", literal)(Window.lessThanOrEqual(_))

    def <=(expr: I ~> W[V]): I ~> W[Boolean] = compareExpr("<=", expr)(Window.lessThanOrEqual(_))

    def >(literal: V): I ~> W[Boolean] = compareLiteral(">", literal)(Window.greaterThan(_))

    def >(expr: I ~> W[V]): I ~> W[Boolean] = compareExpr(">", expr)(Window.greaterThan(_))

    def >=(literal: V): I ~> W[Boolean] = compareLiteral(">=", literal)(Window.greaterThanOrEqual(_))

    def >=(expr: I ~> W[V]): I ~> W[Boolean] = compareExpr(">=", expr)(Window.greaterThanOrEqual(_))
  }
}

final class ConstExprBuilder[A, OP[_]](private val value: A) extends AnyVal {

  def const(implicit op: OP[A]): Expr[Any, A, OP] = Expr.Const(value)
}
