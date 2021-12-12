package com.rallyhealth.vapors.v1

package dsl

import algebra._
import cats.data.NonEmptyList
import cats.{Foldable, Functor}
import data.{FactTypeSet, Justified}
import lens.VariantLens
import logic.Logic

trait JustifiedBuildExprDsl extends BuildExprDsl with WrapJustifiedImplicits with JustifiedDslTypes {

  override protected implicit final def boolLogic: Logic[Justified, Boolean, OP] = Justified.bool[OP]

  override protected implicit final def windowComparable: WindowComparable[Justified, OP] = WindowComparable.justified

  override protected implicit final def extract: Extract[Justified] = Extract.justified

  override protected implicit final def functor: Functor[Justified] = Justified.functor

  override protected implicit final def wrapConst: WrapConst[Justified] = Justified.wrapConst

  override protected implicit final def selectElement: WrapSelected[Justified, OP] = Justified.wrapSelected

  override protected final val defn: WrapDefinitions[Justified, OP] = new WrapDefinitions[Justified, OP]

  // TODO: Should this be visible outside this trait?
  protected def dontShortCircuit: Boolean = false

  override final def ident[I](implicit opI: OP[Justified[I]]): Expr.Identity[Justified[I], OP] = Expr.Identity()

  override final def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opTs: OP[Seq[Justified[T]]],
  ): Expr.ValuesOfType[T, Justified[T], OP] =
    Expr.ValuesOfType[T, Justified[T], OP](factTypeSet, Justified.ByFact(_))

  override implicit def const[A](
    value: A,
  )(implicit
    constType: ConstOutputType[Justified, A],
  ): ConstExprBuilder[constType.Out, OP] =
    new ConstExprBuilder(constType.wrapConst(value))

  override implicit final def in[I, T](expr: I ~:> Justified[T]): JustifiedSelectExprBuilder[I, T] =
    new JustifiedSelectExprBuilder(expr)

  final class JustifiedSelectExprBuilder[-I, A](inputExpr: I ~:> Justified[A]) extends SelectExprBuilder[I, A] {

    override def get[B : Wrappable, O](
      selector: VariantLens.FromTo[A, B],
    )(implicit
      sot: SelectOutputType.Aux[Justified, A, B, O],
      opO: OP[O],
    ): Expr.Select[I, Justified, A, B, O, OP] = Expr.Select(inputExpr, selector(VariantLens.id[A]), sot.wrapSelected)

    override def getAs[C[_]]: GetAsWrapper[I, Justified, A, C, OP] =
      new GetAsWrapper(inputExpr)

  }

  override implicit final def hk[I, C[_], A](
    expr: I ~:> C[Justified[A]],
  )(implicit
    ne: NotEmpty[C, A],
  ): JustifiedHkExprBuilder[I, C, A] =
    new JustifiedHkExprBuilder(expr)

  override final type SpecificHkExprBuilder[-I, C[_], A] = JustifiedHkExprBuilder[I, C, A]

  final class JustifiedHkExprBuilder[-I, C[_], A](inputExpr: I ~:> C[Justified[A]]) extends HkExprBuilder(inputExpr) {

    override def exists(
      conditionExprBuilder: Justified[A] =~:> Justified[Boolean],
    )(implicit
      opO: OP[C[Justified[A]]],
      opA: OP[Justified[A]],
      opB: OP[Justified[Boolean]],
      foldC: Foldable[C],
    ): AndThen[I, C[Justified[A]], Justified[Boolean]] =
      inputExpr.andThen {
        Expr.Exists[C, Justified[A], Justified[Boolean], OP](
          conditionExprBuilder(Expr.Identity()),
          combineTrue = Justified.byInference("exists", true, _),
          combineFalse = NonEmptyList
            .fromList(_)
            .map { justified =>
              Justified.byInference("exists", false, justified)
            }
            .getOrElse {
              // exists in an empty collection is false
              // TODO: Should I put a reason instead of just a const?
              //       Maybe I should pass the original value in these functions?
              Justified.byConst(false)
            },
          dontShortCircuit,
        )
      }

    override def forall(
      conditionExprBuilder: Justified[A] =~:> Justified[Boolean],
    )(implicit
      opO: OP[C[Justified[A]]],
      opA: OP[Justified[A]],
      opB: OP[Justified[Boolean]],
      foldC: Foldable[C],
    ): AndThen[I, C[Justified[A]], Justified[Boolean]] =
      inputExpr.andThen {
        Expr.ForAll[C, Justified[A], Justified[Boolean], OP](
          conditionExprBuilder(Expr.Identity()),
          combineTrue = NonEmptyList
            .fromList(_)
            .map { justified =>
              Justified.byInference("forall", true, justified)
            }
            .getOrElse {
              // forall in an empty collection is true
              // TODO: Should I put a reason instead of just a const?
              Justified.byConst(true)
            },
          combineFalse = Justified.byInference("forall", false, _),
          dontShortCircuit,
        )
      }

    override def map[B](
      mapExprBuilder: Justified[A] =~:> Justified[B],
    )(implicit
      opI: OP[Justified[A]],
      opA: OP[C[Justified[A]]],
      opB: OP[C[Justified[B]]],
      functorC: Functor[C],
    ): AndThen[I, C[Justified[A]], C[Justified[B]]] =
      inputExpr.andThen(Expr.MapEvery[C, Justified[A], Justified[B], OP](mapExprBuilder(ident)))

  }
}

sealed trait WrapJustifiedImplicits extends WrapImplicits with LowPriorityJustifiedWrapImplicits {

  override implicit def constFunctor[C[_] : Functor, O : OP](
    implicit
    aot: ConstOutputType[Justified, O],
  ): ConstOutputType.Aux[Justified, C[O], C[aot.Out]] = defn.constFunctor(aot)

  override implicit def selectFunctor[C[_] : Functor, I : OP, O : OP](
    implicit
    aot: SelectOutputType[Justified, I, O],
  ): SelectOutputType.Aux[Justified, I, C[O], C[aot.Out]] = defn.selectFunctor(aot)
}

sealed trait LowPriorityJustifiedWrapImplicits extends LowPriorityWrapImplicits with JustifiedDslTypes {

  override implicit def constId[O : OP]: ConstOutputType.Aux[Justified, O, Justified[O]] = defn.constId

  override implicit def selectId[I : OP, O : OP]: SelectOutputType.Aux[Justified, I, O, Justified[O]] = defn.selectId
}
