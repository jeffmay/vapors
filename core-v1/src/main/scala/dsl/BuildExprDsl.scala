package com.rallyhealth.vapors.v1

package dsl

import algebra.{CompareWrapped, Expr, Extract, FromConst, WindowComparable}
import data.{FactTypeSet, Window}
import logic.Negation

import cats.{Foldable, Functor, Order}

import scala.annotation.nowarn

trait BuildExprDsl {
  self: DslTypes =>

  protected implicit def wrap: CompareWrapped[W]

  protected implicit def windowComparable: WindowComparable[W, OP]

  protected implicit def extract: Extract[W]

  protected implicit def fromConst: FromConst[W]

  def apply[II, IO <: OI : OPW, OI >: IO, OO : OPW](
    inputExpr: W[II] ~> W[IO],
    outputExpr: W[OI] ~> W[OO],
  ): Expr.AndThen[W[II], W[IO], W[OI], W[OO], OP]

  def ident[I : OPW]: Expr.Identity[W[I], OP]

  def not[I, O : OPW](expr: W[I] ~> W[O])(implicit negation: Negation[W[O]]): Expr.Not[W[I], W[O], OP]

  def valuesOfType[T](factTypeSet: FactTypeSet[T])(implicit opTs: OP[Seq[W[T]]]): Expr.ValuesOfType[T, W[T], OP]

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

//  type SpecificComparisonExprBuilder[I, V] <: ComparisonExprBuilder[I, V]
//
//  implicit def compare[I, V : Order : OP](
//    valueExpr: I ~> W[V],
//  )(implicit
//    opW: OP[W[Window[V]]],
//    opB: OP[W[Boolean]],
//  ): SpecificComparisonExprBuilder[I, V]

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
//    private def compareExpr(
//      name: String,
//      that: Expr[I, W[V], OP],
//    )(
//      using: V => Window[V],
//    )(implicit
//      opWV: OP[W[V]],
//    ): Expr.WithinWindow2[I, V, W, OP] = {
//      Expr.WithinWindow2(
//        valueExpr,
//        Expr.AndThen(
//          that,
//          Expr.CustomFunction[W[V], W[Window[V]], OP](name, CompareWrapped[W].map(_)(using)),
//        ),
//      )
//    }
//
//    protected def compareLiteral(
//      @nowarn name: String, // this is unused but kept for consistency
//      that: V,
//    )(
//      using: V => Window[V],
//    ): Expr.WithinWindow2[I, V, W, OP] =
//      Expr.WithinWindow2(
//        valueExpr,
//        Expr.Const[W[Window[V]], OP](CompareWrapped[W].wrapConst(using(that))),
//      )

    private def compareExpr(
      name: String,
      that: Expr[I, W[V], OP], // TODO: How to compare against another expression?... I need a zip operation
    )(
      using: V => Window[V],
    ): Expr.AndThen[I, W[V], W[V], W[Boolean], OP] = {
      Expr.AndThen(
        valueExpr,
        Expr.WithinWindow3(
          Expr.AndThen(
            that: Expr[Any, W[V], OP], // Doesn't work because that expr requires the same input as the value expr
            Expr.CustomFunction[W[V], W[Window[V]], OP](
              name,
              wrappedValue => {
                val value = Extract[W].extract(wrappedValue)
                val window = using(value)
                val wrappedWindow = FromConst[W].wrapConst(window)
                wrappedWindow
              },
            ),
          ),
        ),
      )
    }

    protected def compareLiteral(
      @nowarn name: String, // this is unused but kept for consistency
      that: V,
    )(
      using: V => Window[V],
    ): Expr.AndThen[I, W[V], W[V], W[Boolean], OP] =
      Expr.AndThen(
        valueExpr,
        Expr.WithinWindow3[V, W, OP](Expr.Const(FromConst[W].wrapConst(using(that)))),
      )

    def <(literal: V): I ~> W[Boolean] = compareLiteral("<", literal)(Window.lessThan(_))

    def >=(literal: V): I ~> W[Boolean] = compareLiteral(">=", literal)(Window.greaterThanOrEqual(_))

    def >=(expr: I ~> W[V]): I ~> W[Boolean] = compareExpr(">=", expr)(Window.greaterThanOrEqual(_))
  }
}

final class ConstExprBuilder[A, OP[_]](private val value: A) extends AnyVal {

  def const(implicit op: OP[A]): Expr[Any, A, OP] = Expr.Const(value)
}
