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
sealed abstract class Expr[-I, +O : OP, OP[_]](val name: String) {

  def visit[G[-_, +_]](v: Expr.Visitor[G, OP]): G[I, O]

  def +[CI <: I, LI >: O, RI >: RO, RO : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    add: Add[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, add.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder[CI, LI, O, RI, RO, add.Out, OP](this, that, add.combine(_, _))
  }
}

/**
  * This class only exists because attempting to require an implicit `PO[AO]` on the [[Expr.+]] method
  * conflicts with the implicit search for [[Add]]. It results in diverging implicit expansion because
  * it can't decide whether to use the implicit `PO[O]` to determine the `AO` type or to use the
  * implicit `Add.Aux[?, ?, O]` to determine the `O` type. This separation ensures that `Add.Aux` is
  * used first to fix the type of `O`, and only afterwards will the compiler search for an implicit
  * `OP[O]` to use when converting this back to an [[Expr]].
  *
  * @note there is an edge case for the `+` method because of the [[any2stringadd]] implicit conversion.
  *       This can go away after this is removed in Scala 3 (and possibly this whole class too!)
  */
final class CombineHolder[-I, -LI, +LO : OP, -RI, +RO : OP, O, OP[_]](
  val left: Expr[I, LO, OP],
  val right: Expr[I, RO, OP],
  val combine: (LI, RI) => O,
)(implicit
  evL: LO <:< LI,
  evR: RO <:< RI,
) {

  /**
    * This only exists because + is implicitly defined on `Any` and requires a `String`, which prevents
    * chaining multiple `+` operators.
    *
    * @see [[Expr.+]] for documentation of functionality.
    */
  def +[CI <: I, NLI >: O, NRI >: NRO, NRO : OP](
    that: Expr[CI, NRO, OP],
  )(implicit
    opO: OP[O],
    add: Add[NLI, NRI],
  ): CombineHolder[CI, NLI, O, NRI, NRO, add.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(toExpr, that, add.combine(_, _))
  }

  def toExpr(implicit opO: OP[O]): Expr[I, O, OP] = Expr.Combine(left, right, combine)
}

object CombineHolder {

  implicit def asExpr[I, O, OP[_]](
    holder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  )(implicit
    opO: OP[O],
  ): Expr[I, O, OP] = holder.toExpr
}

object Expr {

  trait Visitor[~>[-_, +_], OP[_]] {

    def visitCombine[I, LO : OP, RO : OP, LI, RI, O : OP](
      expr: Combine[I, LO, RO, LI, RI, O, OP],
    )(implicit
      evL: LO <:< LI,
      evR: RO <:< RI,
    ): I ~> O

    def visitConst[O : OP](expr: Const[O, OP]): Any ~> O

    def visitExists[I, C[_] : Foldable, E](
      expr: Exists[I, C, E, OP],
    )(implicit
      opC: OP[C[E]],
      opO: OP[Boolean],
    ): I ~> Boolean

    def visitForAll[I, C[_] : Foldable, E](
      expr: ForAll[I, C, E, OP],
    )(implicit
      opC: OP[C[E]],
      opO: OP[Boolean],
    ): I ~> Boolean

    def visitIdentity[I, O : OP](expr: Identity[I, O, OP])(implicit ev: I <:< O): I ~> O

    def visitWithFactValues[T, O : OP](expr: WithFactValues[T, O, OP]): Any ~> O
  }

  final case class Identity[-I, +O : OP, OP[_]]()(implicit ev: I <:< O) extends Expr[I, O, OP]("identity") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitIdentity(this)
  }

  final case class Const[+O : OP, OP[_]](value: O) extends Expr[Any, O, OP]("const") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[Any, O] = v.visitConst(this)
  }

  final case class Combine[-I, +LO : OP, +RO : OP, -LI, -RI, +O : OP, OP[_]](
    leftExpr: Expr[I, LO, OP],
    rightExpr: Expr[I, RO, OP],
    operation: (LI, RI) => O,
  )(implicit
    evL: LO <:< LI,
    evR: RO <:< RI,
  ) extends Expr[I, O, OP]("combine") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitCombine(this)
  }

  final case class Exists[-I, C[_] : Foldable, E, OP[_]](
    inputExpr: Expr[I, C[E], OP],
    conditionExpr: Expr[E, Boolean, OP],
  )(implicit
    opC: OP[C[E]],
    opO: OP[Boolean],
  ) extends Expr[I, Boolean, OP]("exists") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, Boolean] = v.visitExists(this)
  }

  final case class ForAll[-I, C[_] : Foldable, E, OP[_]](
    inputExpr: Expr[I, C[E], OP],
    conditionExpr: Expr[E, Boolean, OP],
  )(implicit
    opC: OP[C[E]],
    opO: OP[Boolean],
  ) extends Expr[I, Boolean, OP]("forall") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, Boolean] = v.visitForAll(this)
  }

  final case class WithFactValues[T, +O : OP, OP[_]](
    factTypeSet: FactTypeSet[T],
    outputExpr: Expr[Seq[T], O, OP],
  ) extends Expr[Any, O, OP]("withFactValues") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[Any, O] = v.visitWithFactValues(this)
  }
}
