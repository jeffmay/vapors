package com.rallyhealth.vapors.v1

package dsl

import algebra.{CompareWrapped, Expr, Extract, FromConst, WindowComparable}
import data.FactTypeSet
import logic.Negation

import cats.{Foldable, Functor}

trait BuildIdExprDsl extends BuildExprDsl with IdExprDsl {

  override implicit final def wrap: CompareWrapped[W] = CompareWrapped.value

  override protected implicit final def windowComparable: WindowComparable[W, OP] = WindowComparable.identity

  override protected implicit final def extract: Extract[W] = Extract.identity

  override protected implicit final def fromConst: FromConst[W] = FromConst.identity

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

  implicit def wrap[A](value: A): ConstExprBuilder[A, OP] = new ConstExprBuilder(value)

  override implicit final def hk[I, C[_], A](expr: I ~> C[A]): HkIdExprBuilder[I, C, A] = new HkIdExprBuilder(expr)

  override final type SpecificHkExprBuilder[I, C[_], A] = HkIdExprBuilder[I, C, A]

  final class HkIdExprBuilder[I, C[_], A](override protected val inputExpr: I ~> C[A]) extends HkExprBuilder[I, C, A] {

    override def exists(
      conditionExpr: A ~> Boolean,
    )(implicit
      opA: OP[C[A]],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): Expr[I, Boolean, OP] =
      Expr.AndThen(
        inputExpr,
        Expr.Exists[C, A, Boolean, OP](conditionExpr, identity, _ => true, _ => false, shortCircuit),
      )

    override def forall(
      conditionExpr: A ~> Boolean,
    )(implicit
      opA: OP[C[A]],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): Expr[I, Boolean, OP] =
      Expr.AndThen(
        inputExpr,
        Expr.ForAll[C, A, Boolean, OP](conditionExpr, identity, _ => true, _ => false, shortCircuit),
      )

    override def map[B](
      mapExpr: A ~> B,
    )(implicit
      opA: OP[C[A]],
      opB: OP[C[B]],
      functorC: Functor[C],
    ): I ~> C[B] =
      Expr.AndThen(inputExpr, Expr.MapEvery[C, A, B, OP](mapExpr))
  }
}
