package com.rallyhealth.vapors.v1

package algebra

import debug.DebugArgs
import lens.VariantLens

/**
  * This class only exists because attempting to require an implicit [[OP]] on a slice or filter method
  * conflicts with the implicit search for the [[dsl.SelectOutputType]]. It results in diverging implicit
  * expansion because it can't decide whether to use the implicit `OP[O]` or to use the implicit
  * `SelectOutputType.Aux[?, ?, O]` to determine the `O` type. This separation ensures that
  * `SelectOutputType.Aux` is used first to fix the type of `O`, and only afterwards will the compiler
  * search for an implicit `OP[O]` to use when converting this back to an [[Expr]].
  *
  * Similar to [[CombineHolder]], but for the result of [[Expr.Select]] operations.
  *
  * @param inputExpr  the expression to run before applying the [[lens]] and [[wrapSelected]] on the result
  * @param wrapSelected a function that defines how to wrap the result of applying the [[lens]].
  *                   If it is a higher-kinded Functor, wrap every element, otherwise wrap the single value directly.
  * @param lens       a lens from the input type to some selected field
  *
  * @tparam I the input to the [[inputExpr]] that is required to produce the output of type [[A]]
  * @tparam A the output of the input expression and the input to the lens
  * @tparam B the output of the lens
  * @tparam O the final computed output type with the wrapper type applied at the appropriate level
  *           (computed using the [[dsl.SelectOutputType]] implicit)* @tparam I
  * @tparam OP the custom output param (see [[dsl.DslTypes.OP]])
  */
final class SelectHolder[-I, A, B, O, OP[_]](
  inputExpr: Expr[I, A, OP],
  lens: VariantLens[A, B],
  wrapSelected: (A, B) => O,
) {

  def toExpr(implicit opO: OP[O]): Expr.Select[I, A, B, O, OP] =
    Expr.Select(inputExpr, lens, wrapSelected)
}

object SelectHolder extends LowPrioritySelectHolderImplicits {

  implicit def asExpr[I, A, B, O, OP[_]](
    holder: SelectHolder[I, A, B, O, OP],
  )(implicit
    opO: OP[O],
  ): Expr.Select[I, A, B, O, OP] = holder.toExpr

  /**
    * This is a hack to fix a reported error by IntelliJ.
    *
    * @see [[CombineHolder.debugAnyHolder]]
    *
    * IntelliJ's presentation compiler infers (wrongly) that the input type of the [[SelectHolder]]
    * is `Nothing`, when it should be `Any`. This hints to the compiler to use `Any` before trying any
    * other type, so this patches an error that prevents you from calling `.run()` on a debugged
    * [[SelectHolder]] produced expression.
    *
    * If this can be removed, then [[LowPrioritySelectHolderImplicits.debugHolder]] can be moved back
    * to this position (and remove the low priority trait if it is empty).
    */
  implicit def debugAnyHolder[A, B, O, OP[_]](
    holder: SelectHolder[Any, A, B, O, OP],
  )(implicit
    opO: OP[O],
    debugArgs: DebugArgs[Expr.Select[Any, A, B, O, OP], OP],
  ): DebugArgs.Attacher[Expr.Select[Any, A, B, O, OP], OP, debugArgs.In, debugArgs.Out] =
    DebugArgs[OP].of(holder.toExpr)(debugArgs)
}

trait LowPrioritySelectHolderImplicits {

  implicit def debugHolder[I, A, B, O, OP[_]](
    holder: SelectHolder[I, A, B, O, OP],
  )(implicit
    opO: OP[O],
    debugArgs: DebugArgs[Expr.Select[I, A, B, O, OP], OP],
  ): DebugArgs.Attacher[Expr.Select[I, A, B, O, OP], OP, debugArgs.In, debugArgs.Out] =
    DebugArgs[OP].of(holder.toExpr)(debugArgs)
}
