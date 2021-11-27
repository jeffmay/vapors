package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.{FactTypeSet, Window}
import lens.VariantLens
import logic.{Conjunction, Disjunction, Logic, Negation}
import math.Power

import cats.{Foldable, Functor, FunctorFilter, Order}
import shapeless.{Generic, HList}

import scala.annotation.nowarn

trait BuildExprDsl extends DebugExprDsl {
  self: DslTypes =>

  protected implicit def boolLogic: Logic[W, Boolean, OP]

  protected implicit def windowComparable: WindowComparable[W, OP]

  protected implicit def extract: Extract[W]

  protected implicit def functor: Functor[W]

  protected implicit def wrapConst: WrapConst[W]

  final def ident[I : OP]: Expr.Identity[I, OP] = Expr.Identity()

  def valuesOfType[T](factTypeSet: FactTypeSet[T])(implicit opTs: OP[Seq[W[T]]]): Expr.ValuesOfType[T, W[T], OP]

  final def pow[I, L, R](
    leftExpr: I ~:> W[L],
    rightExpr: I ~:> W[R],
  )(implicit
    opR: OP[W[R]],
    pow: Power[W[L], W[R]],
  ): CombineHolder[I, W[L], W[L], W[R], W[R], pow.Out, OP] = {
    (leftExpr ^ rightExpr)(opR, pow)
  }

  implicit final def logical[I, B](expr: I ~:> W[B]): LogicalExprOps[I, B, W, OP] = new LogicalExprOps(expr)

  final def and[I, B](
    left: I ~:> W[B],
    right: I ~:> W[B],
  )(implicit
    logic: Conjunction[W, B, OP],
    opO: OP[W[B]],
  ): Expr.And[I, B, W, OP] =
    Expr.And(left, right)

  final def or[I, B](
    left: I ~:> W[B],
    right: I ~:> W[B],
  )(implicit
    logic: Disjunction[W, B, OP],
    opO: OP[W[B]],
  ): Expr.Or[I, B, W, OP] =
    Expr.Or(left, right)

  final def not[I, B](
    expr: I ~:> W[B],
  )(implicit
    negation: Negation[W, B, OP],
    opO: OP[W[B]],
  ): Expr.Not[I, B, W, OP] =
    Expr.Not(expr)

  implicit def wrap[A](value: A)(implicit constType: WrapConstType[W, A]): ConstExprBuilder[constType.Out, OP] =
    new ConstExprBuilder(constType(wrapConst.wrapConst(value)))

  type SpecificSelectExprBuilder[-I, T] <: SelectExprBuilder[I, T]

  implicit def in[I, T](expr: I ~:> W[T]): SpecificSelectExprBuilder[I, T]

  trait SelectExprBuilder[-I, T] extends Any {

    protected def inputExpr: I ~:> W[T]

    def get[O](selector: VariantLens.FromTo[T, O])(implicit opO: OP[W[O]]): I ~:> W[O]
  }

  type SpecificWrapHListExprBuilder[-I, L <: HList] <: WrapHListExprBuilder[I, L]

  implicit def wrapHList[I, L <: HList](expr: I ~:> W[L]): SpecificWrapHListExprBuilder[I, L]

  trait WrapHListExprBuilder[-I, L <: HList] extends Any {

    protected def inputExpr: I ~:> W[L]

    def as[P](
      implicit
      gen: Generic.Aux[P, L],
      opL: OP[W[L]],
      opP: OP[W[P]],
    ): I ~:> W[P]
  }

  implicit def hk[I, C[_], A](expr: I ~:> C[W[A]]): SpecificHkExprBuilder[I, C, A]

  type SpecificHkExprBuilder[-I, C[_], A] <: HkExprBuilder[I, C, A]

  trait HkExprBuilder[-I, C[_], A] extends Any {

    protected def inputExpr: I ~:> C[W[A]]

    def exists(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): Ap[I, C[W[A]], W[Boolean]]

    def filter(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      filterC: FunctorFilter[C],
    ): Ap[I, C[W[A]], C[W[A]]]

    def forall(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): Ap[I, C[W[A]], W[Boolean]]

    def map[B](
      mapExprBuilder: W[A] =~:> W[B],
    )(implicit
      opI: OP[W[A]],
      opA: OP[C[W[A]]],
      opB: OP[C[W[B]]],
      functorC: Functor[C],
    ): Ap[I, C[W[A]], C[W[B]]]
  }

  // TODO: Rename to compareWithinWindow
  implicit def compare[I, V : Order : OP](
    valueExpr: I ~:> W[V],
  )(implicit
    opV: OP[W[V]],
    opW: OP[W[Window[V]]],
    opB: OP[W[Boolean]],
  ): ComparisonExprBuilder[I, V] = new ComparisonExprBuilder(valueExpr)

  // TODO: Rename to WindowComparisonExprBuilder
  class ComparisonExprBuilder[I, V : Order : OP](
    protected val valueExpr: I ~:> W[V],
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
    ): I >=< V =
      Expr.WithinWindow(
        valueExpr,
        Expr.AndThen(
          that,
          Expr.CustomFunction[W[V], W[Window[V]], OP](name, Functor[W].map(_)(using)),
        ),
      )

    protected def compareLiteral(
      @nowarn name: String, // this is unused but kept for consistency
      that: V,
    )(
      using: V => Window[V],
    ): I >=< V =
      Expr.WithinWindow(
        valueExpr,
        Expr.Const[W[Window[V]], OP](WrapConst.wrap(using(that))),
      )

    def <(literal: V): I >=< V = compareLiteral("<", literal)(Window.lessThan(_))

    def <(expr: I ~:> W[V]): I >=< V = compareExpr("<", expr)(Window.lessThan(_))

    def <=(literal: V): I >=< V = compareLiteral("<=", literal)(Window.lessThanOrEqual(_))

    def <=(expr: I ~:> W[V]): I >=< V = compareExpr("<=", expr)(Window.lessThanOrEqual(_))

    def >(literal: V): I >=< V = compareLiteral(">", literal)(Window.greaterThan(_))

    def >(expr: I ~:> W[V]): I >=< V = compareExpr(">", expr)(Window.greaterThan(_))

    def >=(literal: V): I >=< V = compareLiteral(">=", literal)(Window.greaterThanOrEqual(_))

    def >=(expr: I ~:> W[V]): I >=< V = compareExpr(">=", expr)(Window.greaterThanOrEqual(_))

    def within(window: Window[V]): I >=< V = this >=< window

    def >=<(window: Window[V]): I >=< V = Expr.WithinWindow(valueExpr, Expr.Const(WrapConst.wrap(window)))

    def within(expr: I ~:> W[Window[V]]): I >=< V = this >=< expr

    def >=<(expr: I ~:> W[Window[V]]): I >=< V = Expr.WithinWindow(valueExpr, expr)
  }

  implicit def compareIsEqual[I, V : OP](
    valueExpr: I ~:> W[V],
  )(implicit
    compareV: EqualComparable[W, V],
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
  ): EqualComparisonExprBuilder[I, V] =
    new EqualComparisonExprBuilder(valueExpr)

  class EqualComparisonExprBuilder[I, V : OP](
    protected val leftExpr: I ~:> W[V],
  )(implicit
    eqV: EqualComparable[W, V],
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
  ) {

    def ===(literal: V): Expr.IsEqual[I, V, W, OP] = Expr.IsEqual(leftExpr, Expr.Const(WrapConst[W].wrapConst(literal)))

    def ===(rightExpr: I ~:> W[V]): Expr.IsEqual[I, V, W, OP] = Expr.IsEqual(leftExpr, rightExpr)

    def !==(literal: V): Expr.Not[I, Boolean, W, OP] =
      Expr.Not(Expr.IsEqual(leftExpr, Expr.Const(WrapConst[W].wrapConst(literal))))

    def !==(rightExpr: I ~:> W[V]): Expr.Not[I, Boolean, W, OP] =
      Expr.Not(Expr.IsEqual(leftExpr, rightExpr))
  }
}

final class ConstExprBuilder[A, OP[_]](private val value: A) extends AnyVal {

  def const(implicit op: OP[A]): Expr.Const[A, OP] = Expr.Const(value)
}
