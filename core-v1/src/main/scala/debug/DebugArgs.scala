package com.rallyhealth.vapors.v1

package debug

import algebra.Expr
import data.ExprState
import logic.Negation

import scala.reflect.ClassTag

/**
  * A type-level programming trick that enlists the compiler to prove that the [[Debugging]] hook function
  * provided by the developer when constructing the [[Expr]] can be coerced to match the [[ExprState]]
  * provided by the interpreter. By using a separate implicit lookup, the compiler can infer the types
  * automatically for you and apply the appropriate transformation.
  *
  * @see [[Debugging]] for how the types are coerced at runtime.
  */
trait DebugArgs[E <: Expr.AnyWith[OP], OP[_]] {
  type In
  type Out

  def attachHook(
    expr: E,
    hook: ExprState[In, Out] => Unit,
  ): E
}

object DebugArgs {

  type Aux[E <: Expr.AnyWith[OP], DI, DO, OP[_]] = DebugArgs[E, OP] {
    type In = DI
    type Out = DO
  }

  implicit def debugApply[
    AI : ClassTag,
    AO <: BI : OP,
    BI >: AO : ClassTag,
    BO : OP : ClassTag,
    OP[_],
  ]: DebugArgs.Aux[Expr.AndThen[AI, AO, BI, BO, OP], (AI, BI), BO, OP] =
    new DebugArgs[Expr.AndThen[AI, AO, BI, BO, OP], OP] {
      override type In = (AI, BI)
      override type Out = BO
      override def attachHook(
        expr: Expr.AndThen[AI, AO, BI, BO, OP],
        hook: ExprState[(AI, BI), BO] => Unit,
      ): Expr.AndThen[AI, AO, BI, BO, OP] = {
        val debugging = Debugging(hook).ignoreInvalidOutput
        expr.copy(debugging = debugging)
      }
    }

  implicit def debugCombine[
    I : ClassTag,
    LI >: LO : ClassTag,
    LO <: LI : OP,
    RI >: RO : ClassTag,
    RO <: RI : OP,
    O : OP : ClassTag,
    OP[_],
  ]: DebugArgs.Aux[Expr.Combine[I, LI, LO, RI, RO, O, OP], (I, LI, RI), O, OP] =
    new DebugArgs[Expr.Combine[I, LI, LO, RI, RO, O, OP], OP] {
      override type In = (I, LI, RI)
      override type Out = O
      override def attachHook(
        expr: Expr.Combine[I, LI, LO, RI, RO, O, OP],
        hook: ExprState[(I, LI, RI), O] => Unit,
      ): Expr.Combine[I, LI, LO, RI, RO, O, OP] = {
        val debugging = Debugging(hook).ignoreInvalidOutput
        expr.copy(debugging = debugging)
      }
    }

  implicit def debugConst[O : ClassTag : OP, OP[_]]: DebugArgs.Aux[Expr.Const[O, OP], Any, O, OP] =
    new DebugArgs[Expr.Const[O, OP], OP] {
      override type In = Any
      override type Out = O
      override def attachHook(
        expr: Expr.Const[O, OP],
        hook: ExprState[Any, O] => Unit,
      ): Expr.Const[O, OP] = {
        val debugging = Debugging(hook).ignoreInvalidOutput
        expr.copy(debugging = debugging)
      }
    }

  implicit def debugIdent[I : ClassTag : OP, OP[_]]: DebugArgs.Aux[Expr.Identity[I, OP], I, I, OP] =
    new DebugArgs[Expr.Identity[I, OP], OP] {
      override type In = I
      override type Out = I
      override def attachHook(
        expr: Expr.Identity[I, OP],
        hook: ExprState[I, I] => Unit,
      ): Expr.Identity[I, OP] = {
        val debugging = Debugging(hook)
        expr.copy(debugging = debugging)
      }
    }

  implicit def debugNot[I, O : ClassTag : Negation : OP, OP[_]]: DebugArgs.Aux[Expr.Not[I, O, OP], (I, O), O, OP] =
    new DebugArgs[Expr.Not[I, O, OP], OP] {
      override type In = (I, O)
      override type Out = O
      override def attachHook(
        expr: Expr.Not[I, O, OP],
        hook: ExprState[(I, O), O] => Unit,
      ): Expr.Not[I, O, OP] = {
        val debugging = Debugging(hook).ignoreInvalidState
        expr.copy(debugging = debugging)
      }
    }

  implicit def debugValuesOfType[T, O : ClassTag, OP[_]](
    implicit
    op: OP[Seq[O]],
  ): DebugArgs.Aux[Expr.ValuesOfType[T, O, OP], Any, Seq[O], OP] =
    new DebugArgs[Expr.ValuesOfType[T, O, OP], OP] {
      override type In = Any
      override type Out = Seq[O]
      override def attachHook(
        expr: Expr.ValuesOfType[T, O, OP],
        hook: ExprState[Any, Seq[O]] => Unit,
      ): Expr.ValuesOfType[T, O, OP] = {
        val debugging = Debugging(hook).ignoreInvalidOutput
        expr.copy(debugging = debugging)
      }
    }

}
