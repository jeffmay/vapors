package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.{FactTypeSet, Justified}
import logic.Logic

import cats.data.NonEmptyList
import cats.{Foldable, Functor}

trait JustifiedBuildExprDsl extends WrappedBuildExprDsl with JustifiedDslTypes {

  override protected implicit final def boolLogic: Logic[Justified, Boolean, OP] = Justified.bool[OP]

  override protected implicit final def windowComparable: WindowComparable[Justified, OP] = WindowComparable.justified

  override protected implicit final def extract: Extract[Justified] = Extract.justified

  override protected implicit final def functor: Functor[Justified] = Justified.functor

  override protected implicit final def wrapConst: WrapConst[Justified] = WrapConst.justified

  override protected final def wrapElement[C[_], A](
    outer: Justified[C[A]],
    element: A,
  ): Justified[A] =
    Justified.byInference("elementOf", element, NonEmptyList.of(outer))

  // TODO: Should this be visible outside this trait?
  protected def dontShortCircuit: Boolean = false

  override final def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opTs: OP[Seq[Justified[T]]],
  ): Expr.ValuesOfType[T, Justified[T], OP] =
    Expr.ValuesOfType[T, Justified[T], OP](factTypeSet, Justified.ByFact(_))

  override final type SpecificHkExprBuilder[-I, C[_], A] = JustifiedHkExprBuilder[I, C, A]

  override implicit final def hk[I, C[_], A](expr: I ~:> C[Justified[A]]): JustifiedHkExprBuilder[I, C, A] =
    new JustifiedHkExprBuilder(expr)

  final class JustifiedHkExprBuilder[-I, C[_], A](override protected val inputExpr: I ~:> C[Justified[A]])
    extends HkExprBuilder[I, C, A] {

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
