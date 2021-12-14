package com.rallyhealth.vapors.v1

package algebra

import data.ExtractValue.AsBoolean
import data.{ExprState, Extract, Window}
import logic.{Conjunction, Disjunction, Negation}
import cats.data.NonEmptyVector
import cats.{Foldable, Functor}

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
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
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
    * [[com.rallyhealth.vapors.v1.engine.StandardEngine]] visitor.
    *
    * If you only care about the final result, you probably don't need to use this visitor. However, if you want
    * to serialize, interpret, or debug how the result was achieved, you can inspect this tree for visibility.
    *
    * @tparam PO `Previous Output` type (as specific as necessary for applying this visitor)
    * @tparam ~>: an infix type alias for the higher-kinded result type of this visitor
    *             (the symbol was chosen to indicate that the work is performed with the result of the arrow in scope)
    * @tparam OP `Output Parameter` (captured at the definition site for every output type in the expression tree)
    */
  trait Visitor[-PO, ~>:[-_, +_], OP[_]] {

    def visitAnd[I, B, W[+_]](
      result: And[PO, I, B, W, OP],
    )(implicit
      logic: Conjunction[W, B, OP],
      opO: OP[W[B]],
    ): I ~>: W[B]

    def visitAndThen[AI, AO : OP, BI, BO : OP](
      result: AndThen[PO, AI, AO, BI, BO, OP],
    )(implicit
      ev: AO <:< BI,
    ): AI ~>: BO

    def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](result: Combine[PO, I, LI, LO, RI, RO, O, OP]): I ~>: O

    def visitConst[O : OP](result: Const[PO, O, OP]): Any ~>: O

    def visitCustomFunction[I, O : OP](result: CustomFunction[PO, I, O, OP]): I ~>: O

    def visitExists[C[_] : Foldable, A, B : AsBoolean : OP](result: Exists[PO, C, A, B, OP]): C[A] ~>: B

    def visitForAll[C[_] : Foldable, A, B : AsBoolean : OP](result: ForAll[PO, C, A, B, OP]): C[A] ~>: B

    def visitIdentity[I : OP](result: Identity[PO, I, OP]): I ~>: I

    def visitIsEqual[I, V, W[+_]](
      result: IsEqual[PO, I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): I ~>: W[Boolean]

    def visitMapEvery[C[_] : Functor, A, B](result: MapEvery[PO, C, A, B, OP])(implicit opO: OP[C[B]]): C[A] ~>: C[B]

    def visitNot[I, B, W[+_]](
      result: Not[PO, I, B, W, OP],
    )(implicit
      logic: Negation[W, B, OP],
      opB: OP[W[B]],
    ): I ~>: W[B]

    def visitOr[I, B, W[+_]](
      result: Or[PO, I, B, W, OP],
    )(implicit
      logic: Disjunction[W, B, OP],
      opO: OP[W[B]],
    ): I ~>: W[B]

    def visitSelect[I, A, B, O : OP](result: Select[PO, I, A, B, O, OP]): I ~>: O

    def visitValuesOfType[T, O](result: ValuesOfType[PO, T, O, OP])(implicit opTs: OP[Seq[O]]): Any ~>: Seq[O]

    def visitWithinWindow[I, V, W[+_]](
      result: WithinWindow[PO, I, V, W, OP],
    )(implicit
      comparison: WindowComparable[W, OP],
      opB: OP[W[Boolean]],
    ): I ~>: W[Boolean]

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
  final case class Identity[+PO, I : OP, OP[_]](
    expr: Expr.Identity[I, OP],
    state: ExprState[PO, I],
  ) extends ExprResult[PO, I, I, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, I] = v.visitIdentity(this)
  }

  /**
    * The result of running [[Expr.IsEqual]]
    */
  final case class IsEqual[+PO, -I, +V, W[+_], OP[_]](
    expr: Expr.IsEqual[I, V, W, OP],
    state: ExprState[PO, W[Boolean]],
  )(implicit
    eq: EqualComparable[W, V, OP],
    opV: OP[W[V]],
    opO: OP[W[Boolean]],
  ) extends ExprResult[PO, I, W[Boolean], OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, W[Boolean]] = v.visitIsEqual(this)
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
    * The result of running [[Expr.And]]
    */
  final case class And[+PO, -I, +B, W[+_], OP[_]](
    expr: Expr.And[I, B, W, OP],
    state: ExprState[PO, W[B]],
    // TODO: Should I support short-circuiting allowing this to be less than the original length?
    //       Maybe store the first false result and the index? Maybe a vector of optionals?
    results: NonEmptyVector[ExprResult[PO, I, W[B], OP]],
  )(implicit
    logic: Conjunction[W, B, OP],
    opO: OP[W[B]],
  ) extends ExprResult[PO, I, W[B], OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, W[B]] = v.visitAnd(this)
  }

  /**
    * The result of running [[Expr.Or]]
    */
  final case class Or[+PO, -I, +B, W[+_], OP[_]](
    expr: Expr.Or[I, B, W, OP],
    state: ExprState[PO, W[B]],
    // TODO: Should I support short-circuiting allowing this to be less than the original length?
    //       Maybe store the first false result and the index? Maybe a vector of optionals?
    results: NonEmptyVector[ExprResult[PO, I, W[B], OP]],
  )(implicit
    logic: Disjunction[W, B, OP],
    opO: OP[W[B]],
  ) extends ExprResult[PO, I, W[B], OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, W[B]] = v.visitOr(this)
  }

  /**
    * The result of running [[Expr.Not]]
    */
  final case class Not[+PO, -I, +B, W[+_], OP[_]](
    expr: Expr.Not[I, B, W, OP],
    state: ExprState[PO, W[B]],
    inputResult: ExprResult[PO, I, W[B], OP],
  )(implicit
    logic: Negation[W, B, OP],
    opB: OP[W[B]],
  ) extends ExprResult[PO, I, W[B], OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, W[B]] = v.visitNot(this)
  }

  /**
    * The result of running [[Expr.CustomFunction]]
    */
  final case class CustomFunction[+PO, -I, +O : OP, OP[_]](
    expr: Expr.CustomFunction[I, O, OP],
    state: ExprState[PO, O],
  ) extends ExprResult[PO, I, O, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, O] = v.visitCustomFunction(this)
  }

  /**
    * The result of running [[Expr.Exists]]
    */
  final case class Exists[+PO, C[_] : Foldable, A, B : AsBoolean : OP, OP[_]](
    expr: Expr.Exists[C, A, B, OP],
    state: ExprState[PO, B],
    // TODO: Add foundTrueIndex: Option[Int]? conditionResults: C[Boolean]?
  ) extends ExprResult[PO, C[A], B, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[C[A], B] = v.visitExists(this)
  }

  /**
    * The result of running [[Expr.ForAll]]
    */
  final case class ForAll[+PO, C[_] : Foldable, A, B : AsBoolean : OP, OP[_]](
    expr: Expr.ForAll[C, A, B, OP],
    state: ExprState[PO, B],
    // TODO: Add foundTrueIndex: Option[Int]? conditionResults: C[Boolean]?
  ) extends ExprResult[PO, C[A], B, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[C[A], B] = v.visitForAll(this)
  }

  /**
    * The result of running [[Expr.MapEvery]]
    */
  final case class MapEvery[+PO, C[_] : Functor, A, B, OP[_]](
    expr: Expr.MapEvery[C, A, B, OP],
    state: ExprState[PO, C[B]],
    results: C[ExprResult[A, A, B, OP]],
  )(implicit
    opO: OP[C[B]],
  ) extends ExprResult[PO, C[A], C[B], OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[C[A], C[B]] = v.visitMapEvery(this)
  }

  /**
    * The result of running [[Expr.Select]]
    */
  final case class Select[+PO, -I, A, B, +O : OP, OP[_]](
    expr: Expr.Select[I, A, B, O, OP],
    state: ExprState[PO, O],
  ) extends ExprResult[PO, I, O, OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, O] = v.visitSelect(this)
  }

  /**
    * The result of running [[Expr.ValuesOfType]]
    */
  final case class ValuesOfType[+PO, T, +O, OP[_]](
    expr: Expr.ValuesOfType[T, O, OP],
    state: ExprState[PO, Seq[O]],
  )(implicit
    opTs: OP[Seq[O]],
  ) extends ExprResult[PO, Any, Seq[O], OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[Any, Seq[O]] = v.visitValuesOfType(this)
  }

  /**
    * The result of running [[Expr.WithinWindow]]
    */
  final case class WithinWindow[+PO, -I, +V, W[+_], OP[_]](
    expr: Expr.WithinWindow[I, V, W, OP],
    state: ExprState[PO, W[Boolean]],
    valueResult: ExprResult[PO, I, W[V], OP],
    windowResult: ExprResult[PO, I, W[Window[V]], OP],
  )(implicit
    comparison: WindowComparable[W, OP],
    opB: OP[W[Boolean]],
  ) extends ExprResult[PO, I, W[Boolean], OP] {
    override def visit[G[-_, +_]](v: Visitor[PO, G, OP]): G[I, W[Boolean]] = v.visitWithinWindow(this)
  }
}
