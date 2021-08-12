package com.rallyhealth

package vapors.v1.algebra

import vapors.v1.data.FactTypeSet
import vapors.v1.math.Add

import cats.Foldable

/**
  * Required features:
  *
  * - Intermediate caching
  * - Stack-free computation (i.e. conversion to cats / zio)
  * - Interpretation
  * - Reflection
  * - Support for flatMap
  *
  * @tparam I the input value type
  * @tparam O the output value type
  */
sealed trait Expr[-I, +O] {

  def visit[G[-_, +_]](v: Expr.Visitor[G]): G[I, O]

  def +[RI <: I, RO, R](that: Expr[RI, RO])(implicit add: Add[O, RO, R]): Expr[RI, R] =
    Expr.Combine(this, that, (l, r) => add.combine(l, r))
}

object Expr {

  trait Visitor[~>[-_, +_]] {

    def visitCombine[I, LO, RO, LI, RI, O](
      expr: Combine[I, LO, RO, LI, RI, O],
    )(implicit
      evL: LO <:< LI,
      evR: RO <:< RI,
    ): I ~> O
    def visitConst[O](expr: Const[O]): Any ~> O
    def visitExists[I, C[_] : Foldable, E](expr: Exists[I, C, E]): I ~> Boolean
    def visitForAll[I, C[_] : Foldable, E](expr: ForAll[I, C, E]): I ~> Boolean
    def visitIdentity[I, O](expr: Identity[I, O])(implicit ev: I <:< O): I ~> O
    def visitWithFactValues[T, O](expr: WithFactValues[T, O]): Any ~> O
  }

  final case class Identity[-I, +O]()(implicit ev: I <:< O) extends Expr[I, O] {
    override def visit[G[-_, +_]](v: Visitor[G]): G[I, O] = v.visitIdentity(this)
  }

  final case class Const[+O](value: O) extends Expr[Any, O] {
    override def visit[G[-_, +_]](v: Visitor[G]): G[Any, O] = v.visitConst(this)
  }

  final case class Combine[-I, +LO, +RO, -LI, -RI, +O](
    leftExpr: Expr[I, LO],
    rightExpr: Expr[I, RO],
    operation: (LI, RI) => O,
  )(implicit
    evL: LO <:< LI,
    evR: RO <:< RI,
  ) extends Expr[I, O] {
    override def visit[G[-_, +_]](v: Visitor[G]): G[I, O] = v.visitCombine(this)
  }

  final case class Exists[-I, C[_] : Foldable, E](
    inputExpr: Expr[I, C[E]],
    conditionExpr: Expr[E, Boolean],
  ) extends Expr[I, Boolean] {
    override def visit[G[-_, +_]](v: Visitor[G]): G[I, Boolean] = v.visitExists(this)
  }

  final case class ForAll[-I, C[_] : Foldable, E](
    inputExpr: Expr[I, C[E]],
    conditionExpr: Expr[E, Boolean],
  ) extends Expr[I, Boolean] {
    override def visit[G[-_, +_]](v: Visitor[G]): G[I, Boolean] = v.visitForAll(this)
  }

  final case class WithFactValues[T, +O](
    factTypeSet: FactTypeSet[T],
    outputExpr: Expr[Seq[T], O],
  ) extends Expr[Any, O] {
    override def visit[G[-_, +_]](v: Visitor[G]): G[Any, O] = v.visitWithFactValues(this)
  }
}
