package com.rallyhealth.vapors.v1

package algebra

import data.ExprState

import cats.Foldable

/**
  * The result of running the associated [[Expr]] of the same name.
  *
  * This object captures the [[ExprState]] containing the original input (i.e. the previous output `PO` type)
  * as well as the output returned by this expression node `O`.
  *
  * You can use the supplied `OP` type constraint to supply helpful operations for interpreting these results.
  * Typically, you will output care about the final output of the tree, which will be held in the root's
  * `result.state.output` field.
  *
  * @tparam PO Previous Output
  * @tparam I Input
  * @tparam O Output
  * @tparam OP Output Parameter (captured at the definition site for every output type in the expression tree)
  */
sealed abstract class ExprResult[+PO, -I, +O : OP, OP[_]] {

  /**
    * The expression that was used to compute this result.
    *
    * Subclasses should override this with a more specific [[Expr]] subclass of the same name.
    * This can be used to inspect the original expression for details.
    */
  def expr: Expr[I, O, OP]

  /**
    * The expression state at the end of computation. It should contain the previous output (provided
    * as input to this expression) and the output of computing this expression.
    */
  def state: ExprState[PO, O]

  /** @see [[ExprResult.Visitor]] */
  def visit[G[-_, +_]](v: ExprResult.Visitor[PO, G, OP]): G[I, O]
}

object ExprResult {

  /**
    * Similar to the [[Expr.Visitor]], this trait represents the ability to traverse all of the metadata, inputs,
    * outputs, and local state of every expression node that was computed by the
    * [[com.rallyhealth.vapors.interpreter.InterpretExprAsResultFn]] visitor.
    *
    * If you only care about the final result, you probably don't need to use this visitor. However, if you want
    * to serialize, interpret, or debug how the result was achieved, you can inspect this tree for visibility.
    *
    * @tparam PO `Previous Output` type (as specific as necessary for applying this visitor)
    * @tparam ~> an infix type alias for the higher-kinded result type of this visitor
    * @tparam OP `Output Parameter` (captured at the definition site for every output type in the expression tree)
    */
  trait Visitor[-PO, ~>[-_, +_], OP[_]] {

    def visitAnd[I](result: And[PO, I, OP])(implicit opO: OP[Boolean]): I ~> Boolean

    def visitAndThen[AI, AO : OP, BI, BO : OP](
      result: AndThen[PO, AI, AO, BI, BO, OP],
    )(implicit
      ev: AO <:< BI,
    ): AI ~> BO

    def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](result: Combine[PO, I, LI, LO, RI, RO, O, OP]): I ~> O

    def visitConst[O : OP](result: Const[PO, O, OP]): Any ~> O

    def visitExists[C[_] : Foldable, E](
      result: Exists[PO, C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): C[E] ~> Boolean

    def visitForAll[C[_] : Foldable, E](
      result: ForAll[PO, C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): C[E] ~> Boolean

    def visitIdentity[I, O : OP](result: Identity[PO, I, O, OP])(implicit ev: I <:< O): I ~> O

    def visitOr[I](result: Or[PO, I, OP])(implicit opO: OP[Boolean]): I ~> Boolean

    def visitValuesOfType[T](result: ValuesOfType[PO, T, OP])(implicit opTs: OP[Seq[T]]): Any ~> Seq[T]
  }

  /**
    * The result of running [[Expr.Or]]
    */
  final case class And[+PO, -I, OP[_]](
    expr: Expr.And[I, OP],
    state: ExprState[PO, Boolean],
    leftResult: ExprResult[PO, I, Boolean, OP],
    rightResult: ExprResult[PO, I, Boolean, OP],
  )(implicit
    opO: OP[Boolean],
  ) extends ExprResult[PO, I, Boolean, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, Boolean] = v.visitAnd(this)
  }

  /**
    * The result of running [[Expr.Or]]
    */
  final case class Or[+PO, -I, OP[_]](
    expr: Expr.Or[I, OP],
    state: ExprState[PO, Boolean],
    leftResult: ExprResult[PO, I, Boolean, OP],
    rightResult: ExprResult[PO, I, Boolean, OP], // TODO: Should I force this to be strict or allow short-circuiting?
  )(implicit
    opO: OP[Boolean],
  ) extends ExprResult[PO, I, Boolean, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, Boolean] = v.visitOr(this)
  }

  /**
    * The result of running [[Expr.AndThen]]
    */
  final case class AndThen[+PO, -II, +IO : OP, -OI, +OO : OP, OP[_]](
    expr: Expr.AndThen[II, IO, OI, OO, OP],
    state: ExprState[PO, OO],
    inputResult: ExprResult[PO, II, IO, OP],
    outputResult: ExprResult[IO, OI, OO, OP],
  )(implicit
    ev: IO <:< OI,
  ) extends ExprResult[PO, II, OO, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[II, OO] = v.visitAndThen(this)
  }

  /**
    * The result of running [[Expr.Identity]]
    */
  final case class Identity[+PO, -I, +O : OP, OP[_]](
    expr: Expr.Identity[I, O, OP],
    state: ExprState[PO, O],
  )(implicit
    ev: I <:< O,
  ) extends ExprResult[PO, I, O, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, O] = v.visitIdentity(this)
  }

  /**
    * The result of running [[Expr.Const]]
    */
  final case class Const[+PO, +O : OP, OP[_]](
    expr: Expr.Const[O, OP],
    state: ExprState[PO, O],
  ) extends ExprResult[PO, Any, O, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[Any, O] = v.visitConst(this)
  }

  /**
    * The result of running [[Expr.Combine]]
    */
  final case class Combine[+PO, -I, -LI, +LO : OP, -RI, +RO : OP, +O : OP, OP[_]](
    expr: Expr.Combine[I, LI, LO, RI, RO, O, OP],
    state: ExprState[PO, O],
    leftResult: ExprResult[PO, I, LO, OP],
    rightResult: ExprResult[PO, I, RO, OP],
  ) extends ExprResult[PO, I, O, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, O] = v.visitCombine(this)
  }

  /**
    * The result of running [[Expr.Exists]]
    */
  final case class Exists[+PO, C[_] : Foldable, E, OP[_]](
    expr: Expr.Exists[C, E, OP],
    state: ExprState[PO, Boolean],
    // TODO: Add foundTrueIndex: Option[Int]? conditionResults: C[Boolean]?
  )(implicit
    opO: OP[Boolean],
  ) extends ExprResult[PO, C[E], Boolean, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[C[E], Boolean] = v.visitExists(this)
  }

  /**
    * The result of running [[Expr.ForAll]]
    */
  final case class ForAll[+PO, C[_] : Foldable, E, OP[_]](
    expr: Expr.ForAll[C, E, OP],
    state: ExprState[PO, Boolean],
    // TODO: Add foundFalseIndex: Option[Int]? conditionResults: C[Boolean]?
  )(implicit
    opO: OP[Boolean],
  ) extends ExprResult[PO, C[E], Boolean, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[C[E], Boolean] = v.visitForAll(this)
  }

  /**
    * The result of running [[Expr.ValuesOfType]]
    */
  final case class ValuesOfType[+PO, T, OP[_]](
    expr: Expr.ValuesOfType[T, OP],
    state: ExprState[PO, Seq[T]],
  )(implicit
    opTs: OP[Seq[T]],
  ) extends ExprResult[PO, Any, Seq[T], OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[Any, Seq[T]] = v.visitValuesOfType(this)
  }
}
