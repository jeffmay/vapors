package com.rallyhealth

package vapors.v1.algebra

import vapors.v1.data.ExprState

import cats.Foldable

/**
  *
  * @tparam F Effect TODO
  * @tparam PO Previous Input
  * @tparam I Input
  * @tparam O Output
  */
sealed abstract class ExprResult[+PO, -I, +O] {
  def expr: Expr[I, O]
  def state: ExprState[PO, O]
  def visit[G[-_, +_]](v: ExprResult.Visitor[PO, G]): G[I, O]
}

object ExprResult {

  trait Visitor[-PO, ~>[-_, +_]] {
    def visitCombine[I, LO, RO, LI, RI, O](result: Combine[PO, I, LO, RO, LI, RI, O]): I ~> O
    def visitConst[O](result: Const[PO, O]): Any ~> O
    def visitExists[I, C[_] : Foldable, E](result: Exists[PO, I, C, E]): I ~> Boolean
    def visitForAll[I, C[_] : Foldable, E](result: ForAll[PO, I, C, E]): I ~> Boolean
    def visitIdentity[I, O](result: Identity[PO, I, O])(implicit ev: I <:< O): I ~> O
    def visitWithFactValues[T, O](result: WithFactValues[PO, T, O]): Any ~> O
  }

  final case class Identity[+PO, -I, +O](state: ExprState[PO, O])(implicit ev: I <:< O) extends ExprResult[PO, I, O] {
    override val expr: Expr[I, O] = Expr.Identity()
    override def visit[G[-_, +_]](v: Visitor[PO, G]): G[I, O] = v.visitIdentity(this)
  }

  final case class Const[+PO, +O](
    expr: Expr.Const[O],
    state: ExprState[PO, O],
  ) extends ExprResult[PO, Any, O] {
    override def visit[G[-_, +_]](v: Visitor[PO, G]): G[Any, O] = v.visitConst(this)
  }

  final case class Combine[+PO, -I, +LO, +RO, -LI, -RI, +O](
    expr: Expr.Combine[I, LO, RO, LI, RI, O],
    state: ExprState[PO, O],
  ) extends ExprResult[PO, I, O] {
    override def visit[G[-_, +_]](v: Visitor[PO, G]): G[I, O] = v.visitCombine(this)
  }

  final case class Exists[+PO, -I, C[_] : Foldable, E](
    expr: Expr.Exists[I, C, E],
    state: ExprState[PO, Boolean],
  ) extends ExprResult[PO, I, Boolean] {
    override def visit[G[-_, +_]](v: Visitor[PO, G]): G[I, Boolean] = v.visitExists(this)
  }

  final case class ForAll[+PO, -I, C[_] : Foldable, E](
    expr: Expr.ForAll[I, C, E],
    state: ExprState[PO, Boolean],
  ) extends ExprResult[PO, I, Boolean] {
    override def visit[G[-_, +_]](v: Visitor[PO, G]): G[I, Boolean] = v.visitForAll(this)
  }

  final case class WithFactValues[+PO, T, +O](
    expr: Expr.WithFactValues[T, O],
    state: ExprState[PO, O],
  ) extends ExprResult[PO, Any, O] {
    override def visit[G[-_, +_]](v: Visitor[PO, G]): G[Any, O] = v.visitWithFactValues(this)
  }
}
