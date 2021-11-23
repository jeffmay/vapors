package com.rallyhealth.vapors.v1

package dsl

import algebra.{CompareWrapped, Expr, Extract, WindowComparable, WrapConst}
import data.FactTypeSet
import logic.Negation

import cats.{Foldable, Functor}

trait BuildIdExprDsl extends BuildExprDsl with IdExprDsl {

  override implicit final def compareWrapped: CompareWrapped[W] = CompareWrapped.value

  override protected implicit final def windowComparable: WindowComparable[W, OP] = WindowComparable.identity

  override protected implicit final def extract: Extract[W] = Extract.identity

  override protected implicit final def wrapConst: WrapConst[W] = WrapConst.identity

  // TODO: Should this be visible outside this trait?
  protected def shortCircuit: Boolean = true

  override final def apply[AI, AO <: BI : OP, BI >: AO, BO : OP](
    inputExpr: AI ~> AO,
    outputExpr: BI ~> BO,
  ): Expr.AndThen[AI, AO, BI, BO, OP] = Expr.AndThen(inputExpr, outputExpr)

  override final def ident[I : OP]: Expr.Identity[I, OP] = Expr.Identity[I, OP]()

  override final def not[I, O : OPW](expr: I ~> O)(implicit negation: Negation[O]): Expr.Not[I, O, OP] =
    Expr.Not(expr)

  override final def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opTs: OP[Seq[T]],
  ): Expr.ValuesOfType[T, T, OP] =
    Expr.ValuesOfType(factTypeSet, _.value)

  override implicit final def hk[I, C[_], A](expr: I ~> C[A]): HkIdExprBuilder[I, C, A] = new HkIdExprBuilder(expr)

  override final type SpecificHkExprBuilder[I, C[_], A] = HkIdExprBuilder[I, C, A]

  final class HkIdExprBuilder[I, C[_], A](override protected val inputExpr: I ~> C[A]) extends HkExprBuilder[I, C, A] {

    override def exists(
      conditionExprBuilder: A ~~> Boolean,
    )(implicit
      opO: OP[C[A]],
      opA: OP[A],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): Ap[I, C[A], Boolean] =
      inputExpr.andThen(
        Expr.Exists[C, A, Boolean, OP](conditionExprBuilder(ident), _ => true, _ => false, shortCircuit),
      )

    override def forall(
      conditionExprBuilder: A ~~> Boolean,
    )(implicit
      opO: OP[C[A]],
      opA: OP[A],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): Ap[I, C[A], Boolean] =
      inputExpr.andThen(
        Expr.ForAll[C, A, Boolean, OP](conditionExprBuilder(ident), _ => true, _ => false, shortCircuit),
      )

    override def map[B](
      mapExprBuilder: A ~~> B,
    )(implicit
      opI: OP[A],
      opA: OP[C[A]],
      opB: OP[C[B]],
      functorC: Functor[C],
    ): Ap[I, C[A], C[B]] =
      inputExpr.andThen(Expr.MapEvery[C, A, B, OP](mapExprBuilder(ident)))
  }
}
