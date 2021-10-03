package com.rallyhealth.vapors.v1

package dsl

import algebra.{CompareWrapped, Expr, Extract, FromConst, WindowComparable}
import data.{FactTypeSet, Justified, Window}
import logic.Negation

import cats.data.NonEmptyList
import cats.{Foldable, Functor}

trait BuildJustifiedExprDsl extends BuildExprDsl with JustifiedExprDsl {

  override protected implicit final def wrap: CompareWrapped[Justified] = CompareWrapped.justified

  override protected implicit final def windowComparable: WindowComparable[Justified, OP] = WindowComparable.justified

  override protected implicit final def extract: Extract[Justified] = Extract.justified

  override protected implicit final def fromConst: FromConst[Justified] = FromConst.justified

  override def apply[II, IO <: OI : OPW, OI >: IO, OO : OPW](
    inputExpr: Justified[II] ~> Justified[IO],
    outputExpr: Justified[OI] ~> Justified[OO],
  ): Expr.AndThen[Justified[II], Justified[IO], Justified[OI], Justified[OO], OP] =
    Expr.AndThen(inputExpr, outputExpr)

  override def ident[I : OPW]: Expr.Identity[Justified[I], OP] =
    Expr.Identity[Justified[I], OP]()

  override def not[I, O : OPW](
    expr: Justified[I] ~> Justified[O],
  )(implicit
    negation: Negation[Justified[O]],
  ): Expr.Not[Justified[I], Justified[O], OP] =
    Expr.Not(expr)

  override def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opTs: OP[Seq[Justified[T]]],
  ): Expr.ValuesOfType[T, Justified[T], OP] =
    Expr.ValuesOfType[T, Justified[T], OP](factTypeSet, Justified.ByFact(_))

  def value[V : OP]: Justified[V] ~> V =
    Expr.CustomFunction("justifiedValue", _.value)

  implicit def wrapWindow[O](window: Window[O])(implicit opW: OP[Justified[Window[O]]]): Any ~> Justified[Window[O]] =
    Expr.Const(Justified.byConst(window))

  implicit def wrapValue[A](value: A): ConstExprBuilder[Justified[A], OP] =
    new ConstExprBuilder(Justified.byConst(value))

  override type SpecificHkExprBuilder[I, C[_], E] = JustifiedHkExprBuilder[I, C, E]

  override implicit def hk[I, C[_], E](expr: Justified[I] ~> C[Justified[E]]): JustifiedHkExprBuilder[I, C, E] =
    new JustifiedHkExprBuilder(expr)

  final class JustifiedHkExprBuilder[I, C[_], A](override protected val inputExpr: Justified[I] ~> C[Justified[A]])
    extends HkExprBuilder[I, C, A] {

    override def exists(
      conditionExpr: Justified[A] ~> Justified[Boolean],
    )(implicit
      opA: OP[C[Justified[A]]],
      opB: OP[Justified[Boolean]],
      foldC: Foldable[C],
    ): Justified[I] ~> Justified[Boolean] =
      Expr.AndThen(
        inputExpr,
        Expr.Exists[C, Justified[A], Justified[Boolean], OP](
          conditionExpr,
          _.value,
          NonEmptyList
            .fromList(_)
            .map { justified =>
              Justified.byInference("exists", true, justified)
            }
            .getOrElse {
              Justified.byConst(false)
            },
        ),
      )

    // TODO: This expr requires a Boolean output, which seems correct, however, it will require mapping the wrapped
    //       output type to a boolean. For the Justified DSL case, we might want to support an automatic conversion
    //       from Justified[Boolean] into a Boolean through some kind of ExtractValue[Justified[Boolean], Boolean]
    //       concept instance, but we should only do this if there is a clear benefit in either syntax or meaning
    //       because there is a cost in terms of flexibility and clarity.
    override def forall(
      conditionExpr: Justified[A] ~> Boolean,
    )(implicit
      opA: OP[C[Justified[A]]],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): Justified[I] ~> Boolean =
      Expr.AndThen(inputExpr, Expr.ForAll[C, Justified[A], OP](conditionExpr))

    override def map[B](
      mapExpr: Justified[A] ~> Justified[B],
    )(implicit
      opA: OP[C[Justified[A]]],
      opB: OP[C[Justified[B]]],
      functorC: Functor[C],
    ): Justified[I] ~> C[Justified[B]] =
      Expr.AndThen(inputExpr, Expr.MapEvery[C, Justified[A], Justified[B], OP](mapExpr))
  }
}
