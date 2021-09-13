package com.rallyhealth.vapors.v1

package algebra

import debug.DebugArgs
import math.Add

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
  val operationName: String,
  val combine: (LI, RI) => O,
)(implicit
  evLOisLI: LO <:< LI,
  evROisRI: RO <:< RI,
) {

  /**
    * This only exists because + is implicitly defined on `Any` and requires a `String`, which prevents
    * chaining multiple `+` operators.
    *
    * @see [[Expr.+]] for documentation of functionality.
    */
  def +[CI <: I, NLI >: O, NRI >: NRO, NRO <: NRI : OP](
    that: Expr[CI, NRO, OP],
  )(implicit
    opO: OP[O],
    add: Add[NLI, NRI],
  ): CombineHolder[CI, NLI, O, NRI, NRO, add.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(toExpr, that, "add", add.combine(_, _))
  }

  def toExpr(implicit opO: OP[O]): Expr.Combine[I, LI, LO, RI, RO, O, OP] =
    Expr.Combine(left, right, operationName, combine)
}

object CombineHolder {

  implicit def asExpr[I, LI, LO, RI, RO, O, OP[_]](
    holder: CombineHolder[I, LI, LO, RI, RO, O, OP],
  )(implicit
    opO: OP[O],
  ): Expr.Combine[I, LI, LO, RI, RO, O, OP] = holder.toExpr

  implicit def debugExpr[I, LI, LO, RI, RO, O, OP[_]](
    holder: CombineHolder[I, LI, LO, RI, RO, O, OP],
  )(implicit
    opO: OP[O],
    debugArgs: DebugArgs[Expr.Combine[I, LI, LO, RI, RO, O, OP], OP],
  ): Expr.DebugSyntax[Expr.Combine[I, LI, LO, RI, RO, O, OP], debugArgs.In, debugArgs.Out, OP] =
    new Expr.DebugSyntax(debugArgs.attachHook(asExpr(holder), _))
}
