package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import data.{FactTypeSet, Justified}

import cats.Foldable

trait BuildJustifiedExprDsl extends BuildExprDsl with JustifiedExprDsl {

  override def apply[II, IO <: OI : OPW, OI >: IO, OO : OPW](
    inputExpr: Justified[II] ~> Justified[IO],
    outputExpr: Justified[OI] ~> Justified[OO],
  ): Expr.AndThen[Justified[II], Justified[IO], Justified[OI], Justified[OO], OP] =
    Expr.AndThen(inputExpr, outputExpr)

  // TODO: Maybe use a postfix extension method for building constants instead?
//  final def const[O : OPW](value: O): Expr.Const[Justified[O], OP] = Expr.Const(Justified.byConst(value))

//  override final def const[O : OPW](value: Justified[O]): Expr.Const[Justified[O], OP] = Expr.Const(value)

  override def ident[I : OPW]: Expr.Identity[Justified[I], Justified[I], OP] =
    Expr.Identity[Justified[I], Justified[I], OP]()

  override def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opTs: OP[Seq[Justified[T]]],
  ): Expr.ValuesOfType[T, Justified[T], OP] =
    Expr.ValuesOfType[T, Justified[T], OP](factTypeSet, Justified.ByFact(_))

  implicit def wrap[V](value: V): ValueExprBuilder[Justified[V], OP] =
    new ValueExprBuilder(Justified.byConst(value))

  override type SpecificHkExprBuilder[I, C[_], E] = JustifiedHkExprBuilder[I, C, E]

  override implicit def hk[I, C[_], E](expr: Justified[I] ~> C[Justified[E]]): JustifiedHkExprBuilder[I, C, E] =
    new JustifiedHkExprBuilder(expr)

  final class JustifiedHkExprBuilder[I, C[_], E](override protected val inputExpr: Justified[I] ~> C[Justified[E]])
    extends HkExprBuilder[I, C, E] {

    // TODO: This expr requires a Boolean output, which seems correct, however, it will require mapping the wrapped
    //       output type to a boolean. For the Justified DSL case, we might want to support an automatic conversion
    //       from Justified[Boolean] into a Boolean through some kind of ExtractValue[Justified[Boolean], Boolean]
    //       concept instance, but we should only do this if there is a clear benefit in either syntax or meaning
    //       because there is a cost in terms of flexibility and clarity.
    override def exists(
      conditionExpr: Justified[E] ~> Boolean,
    )(implicit
      opCE: OP[C[Justified[E]]],
      opO: OP[Boolean],
      foldC: Foldable[C],
    ): Expr.AndThen[Justified[I], C[Justified[E]], C[Justified[E]], Boolean, OP] = {
      Expr.AndThen(inputExpr, Expr.Exists[C, Justified[E], OP](conditionExpr))
    }
  }
}
