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
sealed abstract class ExprResult[+PO, -I, +O : OP, OP[_]] {
  def expr: Expr[I, O, OP]
  def state: ExprState[PO, O]
  def visit[G[-_, +_]](v: ExprResult.Visitor[PO, G, OP]): G[I, O]
}

object ExprResult {

  trait Visitor[-PO, ~>[-_, +_], OP[_]] {

    def visitCombine[I, LO : OP, RO : OP, LI, RI, O : OP](result: Combine[PO, I, LO, RO, LI, RI, O, OP]): I ~> O

    def visitConst[O : OP](result: Const[PO, O, OP]): Any ~> O

    def visitExists[I, C[_] : Foldable, E](
      result: Exists[PO, I, C, E, OP],
    )(implicit
      opC: OP[C[E]],
      opO: OP[Boolean],
    ): I ~> Boolean

    def visitForAll[I, C[_] : Foldable, E](
      result: ForAll[PO, I, C, E, OP],
    )(implicit
      opC: OP[C[E]],
      opO: OP[Boolean],
    ): I ~> Boolean

    def visitIdentity[I, O : OP](result: Identity[PO, I, O, OP])(implicit ev: I <:< O): I ~> O

    def visitWithFactValues[T, O : OP](result: WithFactValues[PO, T, O, OP]): Any ~> O
  }

  final case class Identity[+PO, -I, +O : OP, OP[_]](state: ExprState[PO, O])(implicit ev: I <:< O)
    extends ExprResult[PO, I, O, OP] {
    override val expr: Expr[I, O, OP] = Expr.Identity()
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, O] = v.visitIdentity(this)
  }

  final case class Const[+PO, +O : OP, OP[_]](
    expr: Expr.Const[O, OP],
    state: ExprState[PO, O],
  ) extends ExprResult[PO, Any, O, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[Any, O] = v.visitConst(this)
  }

  final case class Combine[+PO, -I, +LO : OP, +RO : OP, -LI, -RI, +O : OP, OP[_]](
    expr: Expr.Combine[I, LO, RO, LI, RI, O, OP],
    state: ExprState[PO, O],
    leftResult: ExprResult[PO, I, LO, OP],
    rightResult: ExprResult[PO, I, RO, OP],
  ) extends ExprResult[PO, I, O, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, O] = v.visitCombine(this)
  }

  final case class Exists[+PO, -I, C[_] : Foldable, E, OP[_]](
    expr: Expr.Exists[I, C, E, OP],
    state: ExprState[PO, Boolean],
    inputResult: ExprResult[Any, I, C[E], OP],
    // TODO: Add foundIndex: Option[Int]? conditionResults: C[Boolean]?
  )(implicit
    opC: OP[C[E]],
    opO: OP[Boolean],
  ) extends ExprResult[PO, I, Boolean, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, Boolean] = v.visitExists(this)
  }

  final case class ForAll[+PO, -I, C[_] : Foldable, E, OP[_]](
    expr: Expr.ForAll[I, C, E, OP],
    state: ExprState[PO, Boolean],
  )(implicit
    opC: OP[C[E]],
    opO: OP[Boolean],
  ) extends ExprResult[PO, I, Boolean, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, Boolean] = v.visitForAll(this)
  }

  final case class WithFactValues[+PO, T, +O : OP, OP[_]](
    expr: Expr.WithFactValues[T, O, OP],
    state: ExprState[PO, O],
  ) extends ExprResult[PO, Any, O, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[Any, O] = v.visitWithFactValues(this)
  }
}
