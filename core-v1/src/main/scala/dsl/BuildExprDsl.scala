package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.{Extract, FactTypeSet, Window}
import lens.VariantLens
import logic.{Conjunction, Disjunction, Logic, Negation}
import math.Power

import cats.data.NonEmptyVector
import cats.{Foldable, Functor, FunctorFilter, Order}
import shapeless.{Generic, HList}

trait BuildExprDsl extends DebugExprDsl with WrapArityMethods {
  self: DslTypes with ExprHListDslImplicits with OutputTypeImplicits =>

  /**
    * Use this to implement all implicit `def`s defined by [[ExprHListDslImplicits]]
    */
  protected def defn: DslImplicitDefinitions[W, OP]

  protected implicit def boolLogic: Logic[W, Boolean, OP]

  protected implicit def windowComparable: WindowComparable[W, OP]

  protected implicit def extract: Extract[W]

  protected implicit def wrapConst: WrapConst[W, OP]

  protected implicit def wrapSelected: WrapSelected[W, OP]

  def ident[I](implicit opI: OP[W[I]]): Expr.Identity[W[I], OP]

  def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opT: OP[T],
    opTs: OP[Seq[W[T]]],
  ): Expr.ValuesOfType[T, W[T], OP]

  def pow[I, L, R](
    leftExpr: I ~:> W[L],
    rightExpr: I ~:> W[R],
  )(implicit
    opR: OP[W[R]],
    pow: Power[W[L], W[R]],
  ): CombineHolder[I, W[L], W[L], W[R], W[R], pow.Out, OP]

  implicit final def logical[I, B](expr: I ~:> W[B]): LogicalExprOps[I, B, W, OP] = new LogicalExprOps(expr)

  final def and[I, B](
    left: I ~:> W[B],
    right: I ~:> W[B],
    more: I ~:> W[B]*,
  )(implicit
    logic: Conjunction[W, B, OP],
    opO: OP[W[B]],
  ): Expr.And[I, B, W, OP] =
    Expr.And(left, NonEmptyVector(right, more.toVector))

  final def or[I, B](
    left: I ~:> W[B],
    right: I ~:> W[B],
    more: I ~:> W[B]*,
  )(implicit
    logic: Disjunction[W, B, OP],
    opO: OP[W[B]],
  ): Expr.Or[I, B, W, OP] =
    Expr.Or(left, NonEmptyVector(right, more.toVector))

  final def not[I, B](
    expr: I ~:> W[B],
  )(implicit
    negation: Negation[W, B, OP],
    opO: OP[W[B]],
  ): Expr.Not[I, B, W, OP] =
    Expr.Not(expr)

  implicit def const[A](
    value: A,
  )(implicit
    constType: ConstOutputType[W, A],
  ): ConstExprBuilder[constType.Out, OP]

  implicit def in[I, T](expr: I ~:> W[T]): SelectExprBuilder[I, T]

  abstract class SelectExprBuilder[-I, A](proof: I ~:> W[A]) {

    def get[B : Wrappable, O](
      selector: VariantLens.FromTo[A, B],
    )(implicit
      sot: SelectOutputType.Aux[W, A, B, O],
      opO: OP[O],
    ): Expr.Select[I, W[A], B, O, OP]

    def getAs[C[_]]: GetAsWrapper[I, W, A, C, OP]
  }

  implicit def xhlOps[I, WL <: HList](exprHList: ExprHList[I, WL, OP]): ExprHListOpsBuilder[I, WL]

  /**
    * Operations that can be performed on an [[ExprHList]].
    *
    * @param proof useful for inferring the correct type from the required input expression in subclasses
    *
    * @tparam I the input type
    * @tparam WL the type of [[HList]] of all wrapped outputs of the given [[ExprHList]]
    */
  abstract class ExprHListOpsBuilder[-I, WL <: HList](proof: ExprHList[I, WL, OP]) {

    /**
      * Combine all outputs of all the [[Expr]] nodes into an [[HList]] of the unwrapped elements
      * then wrapped by the wrapper type [[W]].
      *
      * This is useful in combination with the [[ConvertHListExprBuilder.as]] operator to convert
      * an expression of a wrapped [[HList]] into an expression of a wrapped product type.
      *
      * @param isCons evidence that the list is not [[ExprHNil]]
      * @param opO the output parameter of the wrapped output [[UL]]
      *
      * @tparam UL the combined [[HList]] of all unwrapped output types of the embedded [[Expr]] nodes
      *
      * @return an expression from the shared input type to a wrapped [[UL]] [[HList]]s.
      */
    def toHList[UL <: HList](
      implicit
      isCons: ZipToShortest.Aux[W, WL, OP, UL],
      opO: OP[W[UL]],
    ): I ~:> W[UL]

    /**
      * Zip all outputs of all the [[Expr]] nodes into a collection [[C]] of wrapped [[HList]] elements,
      * limited by the length of the shortest collection.
      *
      * This is useful for zipping [[List]]s or [[Option]]s into a single list or option of the dependent parts
      * so that each element can be converted to a product type within the container type.
      *
      * @see [[ZipToShortest]] for details on how this type-level definition is derived.
      *
      * @param zip definition of how to zip the elements of the collection [[C]] with elements of type [[WL]]
      *            into a collection of wrapped [[UL]] elements with the length of the shortest collection.
      * @param opO the output parameter of the collection of wrapped output values of type [[UL]]
      *
      * @tparam C the collection type (covariant because the [[Expr.ZipToShortestHList]] wrapper type is covariant)
      * @tparam UL the combined [[HList]] of all unwrapped output types of the embedded [[Expr]] nodes
      *
      * @return an expression from the shared input type to a collection [[C]] of wrapped [[UL]] [[HList]]s.
      */
    def zipToShortest[C[+_], UL <: HList](
      implicit
      zip: ZipToShortest.Aux[CW[C, W, +*], WL, OP, UL],
      opO: OP[C[W[UL]]],
    ): I ~:> C[W[UL]]
  }

  implicit def fromHL[I, L <: HList](expr: I ~:> W[L]): ConvertHListExprBuilder[I, L]

  abstract class ConvertHListExprBuilder[-I, L <: HList](proof: I ~:> W[L]) {

    /**
      * Convert the wrapped [[HList]] output of type [[L]] from the given expression into the product type [[P]]
      * as defined by the [[Generic]] representation implicitly available from shapeless.
      *
      * @param gen the compiler-provided definition of how to convert the generic representation, [[L]],
      *            into the the product type [[P]]
      * @param opL the unwrapped output parameter for [[L]]
      * @param opWL the wrapped output parameter for [[L]]
      * @param opP the unwrapped output parameter for [[P]]
      * @param opWP the wrapped output parameter for [[P]]
      *
      * @tparam P the product type produced as output. Typically a user-defined case class.
      *
      * @return an expression that converts from a wrapped [[L]] to a wrapped [[P]]
      */
    def as[P](
      implicit
      gen: Generic.Aux[P, L],
      opL: OP[L],
      opWL: OP[W[L]],
      opP: OP[P],
      opWP: OP[W[P]],
    ): AndThen[I, W[L], W[P]]
  }

  implicit def hk[I, C[_], A](expr: I ~:> C[W[A]])(implicit ne: NotEmpty[C, A]): SpecificHkExprBuilder[I, C, A]

  type SpecificHkExprBuilder[-I, C[_], A] <: HkExprBuilder[I, C, A]

  abstract class HkExprBuilder[-I, C[_], A](proof: I ~:> C[W[A]]) {

    def headOption(
      implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[Option[W[A]]],
    ): Expr.Select[I, C[W[A]], Option[W[A]], Option[W[A]], OP]

    def exists(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): AndThen[I, C[W[A]], W[Boolean]]

    def filter(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      filterC: FunctorFilter[C],
    ): AndThen[I, C[W[A]], C[W[A]]]

    def forall(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): AndThen[I, C[W[A]], W[Boolean]]

    def map[B](
      mapExprBuilder: W[A] =~:> W[B],
    )(implicit
      opI: OP[W[A]],
      opA: OP[C[W[A]]],
      opB: OP[C[W[B]]],
      functorC: Functor[C],
    ): AndThen[I, C[W[A]], C[W[B]]]

    def sorted(
      implicit
      sortable: Sortable[C, W[A]],
      opAs: OP[C[W[A]]],
    ): AndThen[I, C[W[A]], C[W[A]]]
  }

  implicit def isInWindow[I, V : Order : OP](
    valueExpr: I ~:> W[V],
  )(implicit
    opV: OP[W[V]],
    opW: OP[Window[V]],
    opB: OP[W[Boolean]],
    opO: OP[W[Window[V]]],
  ): WindowComparisonExprBuilder[I, V] = new WindowComparisonExprBuilder(valueExpr)

  class WindowComparisonExprBuilder[I, V : Order : OP](
    protected val valueExpr: I ~:> W[V],
  )(implicit
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
    opW: OP[Window[V]],
    opO: OP[W[Window[V]]],
  ) {

    private def compareExpr(
      name: String, // TODO: Add the name to the WithinWindow somehow?
      that: Expr[I, W[V], OP],
    )(
      // TODO: Use a lens here? Maybe some kind of "wrap" operation?
      using: V => Window[V],
    ): I >=< V = {
      val lens = VariantLens.id[W[V]].extractValue
      Expr.WithinWindow(
        valueExpr,
        that match {
          case Expr.Const(wv, _) =>
            val v = Extract[W].extract(wv)
            val window = using(v)
            val wrappedWindow = wrapConst.wrapConst(window)
            Expr.Const[W[Window[V]], OP](wrappedWindow)
          case _ =>
            Expr.Select[I, W[V], V, W[Window[V]], OP](
              that,
              lens,
              (wv, a) => wrapSelected.wrapSelected(wv, lens.path, using(a)),
            )
        },
      )
    }

    def <(expr: I ~:> W[V]): I >=< V = compareExpr("<", expr)(Window.lessThan(_))

    def <=(expr: I ~:> W[V]): I >=< V = compareExpr("<=", expr)(Window.lessThanOrEqual(_))

    def >(expr: I ~:> W[V]): I >=< V = compareExpr(">", expr)(Window.greaterThan(_))

    def >=(expr: I ~:> W[V]): I >=< V = compareExpr(">=", expr)(Window.greaterThanOrEqual(_))

    def within(expr: I ~:> W[Window[V]]): I >=< V = this >=< expr

    def >=<(expr: I ~:> W[Window[V]]): I >=< V = Expr.WithinWindow(valueExpr, expr)
  }

  implicit def isEq[I, V : OP](
    valueExpr: I ~:> W[V],
  )(implicit
    compareV: EqualComparable[W, V, OP],
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
  ): EqualComparisonExprBuilder[I, V] =
    new EqualComparisonExprBuilder(valueExpr)

  class EqualComparisonExprBuilder[I, V : OP](
    protected val leftExpr: I ~:> W[V],
  )(implicit
    eqV: EqualComparable[W, V, OP],
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
  ) {

    def ===(rightExpr: I ~:> W[V]): Expr.IsEqual[I, V, W, OP] = Expr.IsEqual(leftExpr, rightExpr)

    def =!=(rightExpr: I ~:> W[V]): Expr.Not[I, Boolean, W, OP] =
      Expr.Not(Expr.IsEqual(leftExpr, rightExpr))
  }
}

final class ConstExprBuilder[A, OP[_]](private val value: A) extends AnyVal {

  def const(implicit op: OP[A]): Expr.Const[A, OP] = Expr.Const(value)
}
