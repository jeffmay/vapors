package com.rallyhealth.vapors.v1

package algebra

import debug.DebugArgs
import math.Add

import scala.annotation.nowarn

/**
  * This class only exists because attempting to require an implicit `PO[LO]` on the [[Expr.+]] method
  * conflicts with the implicit search for [[Add]]. It results in diverging implicit expansion because
  * it can't decide whether to use the implicit `PO[O]` to determine the `LO` type or to use the
  * implicit `Add.Aux[?, ?, O]` to determine the `O` type. This separation ensures that `Add.Aux` is
  * used first to fix the type of `O`, and only afterwards will the compiler search for an implicit
  * `OP[O]` to use when converting this back to an [[Expr]].
  *
  * @note there is an edge case for the `+` method because of the [[any2stringadd]] implicit conversion.
  *       This can go away after this is removed in Scala 3 (and possibly this whole class too!)
  *
  * @tparam I the input to both the left and right expressions
  * @tparam LI the input to the left-side of the operation
  * @tparam LO the output of the left-side expression (must be a subtype of `LI`)
  * @tparam RI the input to the right-side of the operation
  * @tparam RO the output of the right-side expression (must be a subtype of `RI`)
  * @tparam O the output of the operation (and of the expression produced by [[toExpr]])
  * @tparam OP the custom output param (see [[dsl.DslTypes.OP]])
  *
  * @param evLOisLI compiler-supplied evidence that `LO` is a subtype of `LI`
  * @param evROisRI compiler-supplied evidence that `RO` is a subtype of `RI`
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
  def +[CI <: I, NLI >: O, NRI >: NRO, NRO : OP](
    that: Expr[CI, NRO, OP],
  )(implicit
    opO: OP[O],
    add: Add[NLI, NRI],
  ): CombineHolder[CI, NLI, O, NRI, NRO, add.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(toExpr, that, "add", add.add(_, _): @nowarn)
  }

  def toExpr(implicit opO: OP[O]): Expr.Combine[I, LI, LO, RI, RO, O, OP] =
    Expr.Combine(left, right, operationName, combine)
}

object CombineHolder extends LowPriorityCombineHolderImplicits {

  implicit def asExpr[I, LI, LO, RI, RO, O, OP[_]](
    holder: CombineHolder[I, LI, LO, RI, RO, O, OP],
  )(implicit
    opO: OP[O],
  ): Expr.Combine[I, LI, LO, RI, RO, O, OP] = holder.toExpr

  /**
    * This is a hack to fix a reported error by IntelliJ.
    *
    * @see [[SelectHolder.debugAnyHolder]]
    *
    * IntelliJ's presentation compiler infers (wrongly) that the input type of the [[CombineHolder]]
    * is `Nothing`, when it should be `Any`. This hints to the compiler to use `Any` before trying any
    * other type, so this patches an error that prevents you from calling `.run()` on a debugged
    * [[CombineHolder]] produced expression.
    *
    * If this can be removed, then [[LowPriorityCombineHolderImplicits.debugHolder]] can be moved back
    * to this position (and remove the low priority trait if it is empty).
    */
  implicit def debugAnyHolder[LI, LO, RI, RO, O, OP[_]](
    holder: CombineHolder[Any, LI, LO, RI, RO, O, OP],
  )(implicit
    opO: OP[O],
    debugArgs: DebugArgs[Expr.Combine[Any, LI, LO, RI, RO, O, OP], OP],
  ): DebugArgs.Attacher[Expr.Combine[Any, LI, LO, RI, RO, O, OP], OP, debugArgs.In, debugArgs.Out] =
    DebugArgs[OP].of(holder.toExpr)(debugArgs)
}

trait LowPriorityCombineHolderImplicits {

  implicit def debugHolder[I, LI, LO, RI, RO, O, OP[_]](
    holder: CombineHolder[I, LI, LO, RI, RO, O, OP],
  )(implicit
    opO: OP[O],
    debugArgs: DebugArgs[Expr.Combine[I, LI, LO, RI, RO, O, OP], OP],
  ): DebugArgs.Attacher[Expr.Combine[I, LI, LO, RI, RO, O, OP], OP, debugArgs.In, debugArgs.Out] =
    DebugArgs[OP].of(holder.toExpr)(debugArgs)
}
