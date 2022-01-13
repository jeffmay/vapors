package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data._
import lens.{CollectInto, IterableInto, VariantLens}
import logic.{Conjunction, Disjunction, Logic, Negation}
import math.{Add, Power}

import cats.data.{NonEmptySeq, NonEmptyVector}
import cats.{Applicative, FlatMap, Foldable, Functor, Id, Order, Reducible, SemigroupK, Traverse}
import shapeless.{Generic, HList}

trait BuildExprDsl
  extends DebugExprDsl
  with SliceRangeSyntax
  with TimeFunctions
  with WrapArityMethods
  with UsingDefinitionArityMethods {
  self: DslTypes with ExprHListDslImplicits with OutputTypeImplicits =>

  /**
    * Use this to implement all implicit `def`s defined by [[ExprHListDslImplicits]]
    */
  protected def defn: DslImplicitDefinitions[W, OP]

  protected implicit def boolLogic: Logic[W, Boolean, OP]

  protected implicit def windowComparable: WindowComparable[W, OP]

  protected implicit def extract: Extract[W]

  protected implicit def wrapConst: WrapConst[W, OP]

  protected implicit def wrapContained: WrapContained[W, OP]

  protected implicit def wrapSelected: WrapSelected[W, OP]

  def ident[I](implicit opI: OP[W[I]]): Expr.Identity[W[I], OP]

  final def concat[C[_] : Foldable, I, A](
    expressions: I ~:> C[A]*,
  )(implicit
    opSSA: OP[Seq[Seq[A]]],
    opSA: OP[Seq[A]],
  ): AndThen[I, Seq[Seq[A]], Seq[A]] = {
    Expr
      .Sequence(expressions.map {
        Expr.Select(_, VariantLens.id[C[A]].to[Seq], (_: C[A], sa: Seq[A]) => sa)
      })
      .andThen(Expr.Flatten())
  }

  def seq[I, O](expressions: I ~:> W[O]*)(implicit opO: OP[Seq[W[O]]]): I ~:> Seq[W[O]] =
    wrapAll(expressions)

  def some[I, O](expr: I ~:> W[O])(implicit opO: OP[Option[W[O]]]): I ~:> Option[W[O]] =
    wrapAll(Option(expr))

  def none[O](implicit opO: OP[Option[W[O]]]): Any ~:> Option[W[O]] = Expr.Const(None: Option[W[O]])

  def define[T](factType: FactType[T]): DefineBuilder[T]

  abstract class DefineBuilder[T](factType: FactType[T]) {

    def oneFrom(
      defnExpr: Any ~:> W[T],
    )(implicit
      opWT: OP[W[T]],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[Any, Id, T, OP]

    def from[C[_] : Functor : Foldable](
      defnExpr: Any ~:> C[W[T]],
    )(implicit
      opWT: OP[W[T]],
      opCT: OP[C[T]],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[Any, C, T, OP]

    def fromInput[I, C[_] : Functor : Foldable](
      buildDefnExpr: I =~:> C[W[T]],
    )(implicit
      opI: OP[I],
      opWT: OP[W[T]],
      opCT: OP[C[T]],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[I, C, T, OP]

    final def oneFromConst(
      value: T,
    )(implicit
      opCT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[Any, Id, T, OP] =
      Expr.Define(factType, Expr.Const(value): Any ~:> Id[T])

    final def fromConst[C[_] : Foldable](
      values: C[T],
    )(implicit
      opT: OP[Seq[T]],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[Any, Seq, T, OP] =
      Expr.Define(factType, Expr.Const(Foldable[C].toList(values): Seq[T]))
  }

  @deprecated(
    """You should use the using().thenReturn(...) DSL method instead.
    
You should prefer put your declaration of dependency on definitions close to where you actually use them.""",
  )
  final def usingDefinitions[I, O](
    definitions: Expr.Definition[I, OP]*,
  )(
    thenExpr: Any ~:> O,
  )(implicit
    opO: OP[O],
  ): Expr.UsingDefinitions[I, O, OP] =
    Expr.UsingDefinitions(definitions, thenExpr)

  def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opT: OP[T],
    opTs: OP[Seq[W[T]]],
  ): Expr.ValuesOfType[T, W[T], OP]

  def min[I, N : Order](
    first: I ~:> W[N],
    rest: I ~:> W[N]*,
  )(implicit
    opSWN: OP[NonEmptySeq[W[N]]],
    opWN: OP[W[N]],
  ): I ~:> W[N] =
    wrapAll(NonEmptySeq(first, rest)).min

  def max[I, N : Order](
    first: I ~:> W[N],
    rest: I ~:> W[N]*,
  )(implicit
    opSWN: OP[NonEmptySeq[W[N]]],
    opWN: OP[W[N]],
  ): I ~:> W[N] =
    wrapAll(NonEmptySeq(first, rest)).max

  def sum[I, N : Numeric](
    first: I ~:> W[N],
    rest: I ~:> W[N]*,
  )(implicit
    addWN: Add.Id[W[N]],
    opSWN: OP[NonEmptySeq[W[N]]],
    opN: OP[N],
    opTWN: OP[(W[N], W[N])],
    opWN: OP[W[N]],
  ): I ~:> W[N] = wrapAll(NonEmptySeq(first, rest)).sum

  def pow[I, L, R](
    leftExpr: I ~:> W[L],
    rightExpr: I ~:> W[R],
  )(implicit
    opR: OP[W[R]],
    pow: Power[W[L], W[R]],
  ): CombineHolder[I, W[L], W[L], W[R], W[R], pow.Out, OP]

  final def wrapAll[C[+_] : Applicative : SemigroupK : Traverse, I, O](
    expressions: C[I ~:> O],
  )(implicit
    opCO: OP[C[O]],
  ): Expr.Sequence[C, I, O, OP] =
    Expr.Sequence(expressions)

  def when[I](condExpr: I ~:> W[Boolean]): WhenBuilder[I, W[Boolean]]

  abstract class WhenBuilder[-I, B : ExtractValue.AsBoolean](firstCondExpr: I ~:> B) {
    def thenReturn[TI <: I, O](thenExpr: TI ~:> O): WhenElseBuilder[TI, B, O]
  }

  abstract class WhenElifBuilder[-I, B : ExtractValue.AsBoolean, +O](
    branches: NonEmptySeq[Expr.ConditionBranch[I, B, O, OP]],
    nextCondExpr: I ~:> B,
  ) {
    def thenReturn[TI <: I, TO >: O](thenExpr: TI ~:> TO): WhenElseBuilder[TI, B, TO]
  }

  abstract class WhenElseBuilder[-I, B : ExtractValue.AsBoolean, +O](
    branches: NonEmptySeq[Expr.ConditionBranch[I, B, O, OP]],
  ) {

    def elif[CI <: I](nextCondExpr: CI ~:> B): WhenElifBuilder[CI, B, O]

    def elseReturn[TI <: I, TO >: O](
      elseExpr: TI ~:> TO,
    )(implicit
      opO: OP[TO],
    ): Expr.When[TI, W[Boolean], TO, OP]
  }

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

  // TODO: Is this redundant syntax worth keeping around?
  implicit def inSet[I, A](inputExpr: I ~:> W[A]): InSetExprBuilder[I, A]

  abstract class InSetExprBuilder[-I, +A](proof: I ~:> W[A]) {

    def in[NI <: I, V >: A](
      validValuesExpr: NI ~:> Set[W[V]],
    )(implicit
      opA: OP[V],
      opO: OP[W[Boolean]],
    ): Expr.ContainsAny[NI, W, Id, V, W[Boolean], OP]
  }

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

  implicit final def flat[I, C[_], D[_], A](inputExpr: I ~:> C[D[A]]): FlattenExprBuilder[I, C, D, A] =
    new FlattenExprBuilder(inputExpr)

  final class FlattenExprBuilder[-I, C[_], D[_], A](inputExpr: I ~:> C[D[A]]) {

    def flatten[F[a] >: C[a]](
      implicit
      flatMapC: FlatMap[F],
      ev: C[D[A]] <:< F[F[A]],
      opF: OP[F[F[A]]],
      opO: OP[F[A]],
    ): I ~:> F[A] = {
      val lens = VariantLens.id[C[D[A]]].as[F[F[A]]]
      Expr.Select[I, C[D[A]], F[F[A]], F[F[A]], OP](inputExpr, lens, (_, res) => res).andThen(Expr.Flatten[F, A, OP]())
    }
  }

  implicit def optionOps[I, O](optExpr: I ~:> Option[W[O]]): OptionExprBuilder[I, O] =
    new OptionExprBuilder(optExpr)

  class OptionExprBuilder[-I, +O](optExpr: I ~:> Option[W[O]]) {

    def getOrElse[EI <: I, EO >: O](defaultExpr: EI ~:> W[EO])(implicit opO: OP[W[EO]]): Expr.GetOrElse[EI, W[EO], OP] =
      Expr.GetOrElse(optExpr, defaultExpr)
  }

  implicit def sizeOf[I, C](expr: I ~:> C): SizeOfExprBuilder[I, C]

  abstract class SizeOfExprBuilder[-I, C](proof: I ~:> C) {

    def isEmpty(
      implicit
      sizeCompare: SizeComparable[C, W[Int], W[Boolean]],
      opI: OP[Int],
      opWI: OP[W[Int]],
      opWB: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]]

    def sizeIs: SizeIsBuilder[I, C]
  }

  implicit def hk[I, C[_], A](expr: I ~:> C[W[A]])(implicit ne: NotEmpty[C, A]): HkExprBuilder[I, C, A]

  abstract class HkExprBuilder[-I, C[_], A](proof: I ~:> C[W[A]]) {

    def atIndex(
      index: Long,
    )(implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[Option[W[A]]],
    ): Expr.Select[I, C[W[A]], Option[W[A]], Option[W[A]], OP]

    def containsAny[NI <: I](
      validValuesExpr: NI ~:> Set[W[A]],
    )(implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[W[Boolean]],
    ): Expr.ContainsAny[NI, W, C, A, W[Boolean], OP]

    def head(
      implicit
      reducibleC: Reducible[C],
      opA: OP[W[A]],
    ): Expr.Select[I, C[W[A]], W[A], W[A], OP]

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

    def filter[D[_]](
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      filter: CollectInto.Filter[C, W[A], D],
      opA: OP[W[A]],
      opB: OP[D[W[A]]],
    ): AndThen[I, C[W[A]], D[W[A]]]

    def forall(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): AndThen[I, C[W[A]], W[Boolean]]

    def foldLeft[CI <: I, B](
      initExpr: CI ~:> W[B],
    )(
      foldExprBuilder: ((W[B], W[A]) ~:> W[B], (W[B], W[A]) ~:> W[A]) => ((W[B], W[A]) ~:> W[B]),
    )(implicit
      foldableC: Foldable[C],
      opBA: OP[(W[B], W[A])],
      opA: OP[W[A]],
      opB: OP[W[B]],
    ): Expr.FoldLeft[CI, C, W[A], W[B], OP]

    def map[B](
      mapExprBuilder: W[A] =~:> W[B],
    )(implicit
      opI: OP[W[A]],
      opA: OP[C[W[A]]],
      opB: OP[C[W[B]]],
      functorC: Functor[C],
    ): AndThen[I, C[W[A]], C[W[B]]]

    def flatMap[D[a] >: C[a] : FlatMap, O](
      exprBuilder: W[A] =~:> D[W[O]],
    )(implicit
      opA: OP[W[A]],
      opDDO: OP[D[D[W[O]]]],
      opDO: OP[D[W[O]]],
    ): AndThen[I, D[D[W[O]]], D[W[O]]]

    def min(
      implicit
      reducibleC: Reducible[C],
      orderA: Order[A],
      opO: OP[W[A]],
    ): AndThen[I, C[W[A]], W[A]]

    def max(
      implicit
      reducibleC: Reducible[C],
      orderA: Order[A],
      opO: OP[W[A]],
    ): AndThen[I, C[W[A]], W[A]]

    def isEmpty(
      implicit
      sizeCompare: SizeComparable[C[W[A]], W[Int], W[Boolean]],
      opI: OP[Int],
      opWI: OP[W[Int]],
      opWB: OP[W[Boolean]],
    ): AndThen[I, C[W[A]], W[Boolean]]

    def sizeIs: SizeIsBuilder[I, C[W[A]]]

    def slice[D[_]](
      range: SliceRange.Relative,
    )(implicit
      traverseC: Traverse[C],
      filter: CollectInto.Filter[C, W[A], D],
      opO: OP[D[W[A]]],
    ): AndThen[I, C[W[A]], D[W[A]]]

    def sorted(
      implicit
      sortable: Sortable[C, W[A]],
      opAs: OP[C[W[A]]],
    ): AndThen[I, C[W[A]], C[W[A]]]

    def sum(
      implicit
      foldableC: Foldable[C],
      addA: Add.Id[W[A]],
      numericA: Numeric[A],
      opA: OP[A],
      opAA: OP[(W[A], W[A])],
      opO: OP[W[A]],
    ): Expr.FoldLeft[I, C, W[A], W[A], OP] =
      this.foldLeft(Expr.Const[W[A], OP](wrapConst.wrapConst(numericA.zero)))(_ + _)

    def to[S[_]](
      implicit
      foldableC: Foldable[C],
      into: IterableInto[S, W[A]],
    ): SelectHolder[I, C[W[A]], into.Out, into.Out, OP]
  }

  abstract class SizeIsBuilder[-I, C](proof: I ~:> C) {

    def ===(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]]

    def >(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]]

    def >=(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]]

    def <(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]]

    def <=(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]]
  }

  implicit def isInWindow[I, V : Order : OP](
    valueExpr: I ~:> W[V],
  )(implicit
    opV: OP[W[V]],
    opW: OP[Window[V]],
    opB: OP[W[Boolean]],
    opO: OP[W[Window[V]]],
  ): WindowComparisonExprBuilder[I, V] = new WindowComparisonExprBuilder(valueExpr)

  class WindowComparisonExprBuilder[-I, V : Order : OP](
    protected val lhsExpr: I ~:> W[V],
  )(implicit
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
    opW: OP[Window[V]],
    opO: OP[W[Window[V]]],
  ) {

    private def compareExpr[NI <: I](
      name: String, // TODO: Add the name to the WithinWindow somehow?
      that: Expr[NI, W[V], OP],
    )(
      // TODO: Use a lens here? Maybe some kind of "wrap" operation?
      using: V => Window[V],
    ): NI >=< V = {
      val lens = VariantLens.id[W[V]].extractValue
      Expr.WithinWindow(
        lhsExpr,
        that match {
          case Expr.Const(wv, _) =>
            val v = Extract[W].extract(wv)
            val window = using(v)
            val wrappedWindow = wrapConst.wrapConst(window)
            Expr.Const[W[Window[V]], OP](wrappedWindow)
          case _ =>
            Expr.Select[NI, W[V], V, W[Window[V]], OP](
              that,
              lens,
              (wv, a) => wrapSelected.wrapSelected(wv, lens.path, using(a)),
            )
        },
      )
    }

    def <[NI <: I](expr: NI ~:> W[V]): NI >=< V = compareExpr("<", expr)(Window.lessThan(_))

    def <=[NI <: I](expr: NI ~:> W[V]): NI >=< V = compareExpr("<=", expr)(Window.lessThanOrEqual(_))

    def >[NI <: I](expr: NI ~:> W[V]): NI >=< V = compareExpr(">", expr)(Window.greaterThan(_))

    def >=[NI <: I](expr: NI ~:> W[V]): NI >=< V = compareExpr(">=", expr)(Window.greaterThanOrEqual(_))

    def within[NI <: I](expr: NI ~:> W[Window[V]]): NI >=< V = this >=< expr

    def >=<[NI <: I](expr: NI ~:> W[Window[V]]): NI >=< V = Expr.WithinWindow(lhsExpr, expr)
  }

  implicit def isEq[I, V : OP](
    valueExpr: I ~:> W[V],
  )(implicit
    compareV: EqualComparable[W, V, OP],
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
  ): EqualComparisonExprBuilder[I, V] =
    new EqualComparisonExprBuilder(valueExpr)

  class EqualComparisonExprBuilder[-I, V : OP](
    protected val leftExpr: I ~:> W[V],
  )(implicit
    eqV: EqualComparable[W, V, OP],
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
  ) {

    def ===[NI <: I](rightExpr: NI ~:> W[V]): Expr.IsEqual[NI, V, W, OP] = Expr.IsEqual(leftExpr, rightExpr)

    def =!=[NI <: I](rightExpr: NI ~:> W[V]): Expr.Not[NI, Boolean, W, OP] =
      Expr.Not(Expr.IsEqual(leftExpr, rightExpr))
  }
}

final class ConstExprBuilder[A, OP[_]](private val value: A) extends AnyVal {

  def const(implicit op: OP[A]): Expr.Const[A, OP] = Expr.Const(value)
}
