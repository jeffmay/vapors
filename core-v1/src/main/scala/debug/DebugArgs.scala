package com.rallyhealth.vapors.v1

package debug

import algebra.Expr
import data.{ExprState, Window}
import lens.VariantLens

import cats.data.NonEmptyList
import izumi.reflect.Tag

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
  type Out // TODO: Is this ever NOT just the 'O' type parameter of Expr[I, O, OP]?
}

object DebugArgs {

  type Aux[E <: Expr.AnyWith[OP], OP[_], DI, DO] = DebugArgs[E, OP] {
    type In = DI
    type Out = DO
  }

  def apply[OP[_]]: Using[OP] = new Using

  final class Using[OP[_]](private val dummy: Boolean = true) extends AnyVal {

    def of[E <: Expr.AnyWith[OP]](
      expr: E,
    )(implicit
      debugArgs: DebugArgs[E, OP],
    ): Adapter[E, OP, debugArgs.In, debugArgs.Out] = new Adapter(expr)
  }

  /**
    * Uses the fixed types supplied by the [[DebugArgs]] compiler trick to provide better type inference
    * when attaching a hook.
    *
    * @tparam E The specific [[Expr]] subclass used to compute the debug types
    * @tparam DI The `Debugging Input` type (this doesn't always need to match the actual input to the [[Expr]])
    *            Typically, this is a tuple of all of the input types of any sub-expressions preceded by the
    *            input to the given expression.
    * @tparam DO The `Debugging Output` type (this is always the same as the [[Expr]]'s output type)
    * @tparam OP The `Output Parameter` (captured at the definition site for every output type in the expression tree)
    */
  sealed trait Attacher[E <: Expr.AnyWith[OP], OP[_], DI, DO] extends Any {

    /**
      * Attach the given function to run when the expression is being interpreted. It allows you to intercept
      * the computation and inspect the current state, but it does not allow you to alter it.
      *
      * @see [[Expr.withDebugging]]
      * @see [[Debugging]]
      *
      * @note The [[ClassTag]]s are required to make sure the expression is not casted improperly.
      *       The compiler thinks it may be possible to cast an expression in such a way as to make
      *       the attached hook require a more specific type than the expression will evaluate.
      *       If this happens, the library will throw a more helpful error than a [[ClassCastException]].
      *
      *       It may also be the case that this scenario is impossible, in which case, these class
      *       tags may be removed in the future.
      */
    def debug(
      hook: ExprState[DI, DO] => Unit,
    )(implicit
      cti: ClassTag[DI],
      tti: Tag[DI],
      cto: ClassTag[DO],
      tto: Tag[DO],
    ): E
  }

  sealed trait Invoker[E <: Expr.AnyWith[OP], OP[_], DI, DO] extends Any {

    def invokeDebugger(args: ExprState[DI, DO]): Unit
  }

  final class Adapter[E <: Expr.AnyWith[OP], OP[_], DI, DO] private[DebugArgs] (private val expr: E)
    extends AnyVal
    with Attacher[E, OP, DI, DO]
    with Invoker[E, OP, DI, DO] {

    override def debug(
      hook: ExprState[DI, DO] => Unit,
    )(implicit
      cti: ClassTag[DI],
      tti: Tag[DI],
      cto: ClassTag[DO],
      tto: Tag[DO],
    ): E = {
      val expectedClass = expr.getClass
      val debugging = Debugging(hook).throwOnInvalidState
      val debuggedExpr = expr.withDebugging(debugging)
      if (expectedClass.isInstance(debuggedExpr)) {
        debuggedExpr.asInstanceOf[E]
      } else {
        throw new IllegalStateException(
          s"Expr.${expectedClass.getSimpleName}.withDebugging() produced the wrong type of Expr (${debuggedExpr.getClass.getSimpleName})",
        )
      }

    }

    override def invokeDebugger(args: ExprState[DI, DO]): Unit = expr.debugging.throwOnInvalidState.attach(args)
  }

  implicit def anyExpr[I, O, OP[_]]: Aux[Expr[I, O, OP], OP, Any, O] =
    new DebugArgs[Expr[I, O, OP], OP] {
      override type In = Any
      override type Out = O
    }

  implicit def debugAndThen[II, IO, OI, OO, OP[_]](
    implicit
    evIOisOI: IO <:< OI,
  ): Aux[Expr.AndThen[II, IO, OI, OO, OP], OP, (II, OI), OO] =
    new DebugArgs[Expr.AndThen[II, IO, OI, OO, OP], OP] {
      override type In = (II, OI)
      override type Out = OO
    }

  implicit def debugAnd[I, OP[_]]: Aux[Expr.And[I, OP], OP, (I, Boolean, Boolean), Boolean] =
    new DebugArgs[Expr.And[I, OP], OP] {
      override type In = (I, Boolean, Boolean)
      override type Out = Boolean
    }

  implicit def debugCustomFunction[I, O, OP[_]]: Aux[Expr.CustomFunction[I, O, OP], OP, I, O] =
    new DebugArgs[Expr.CustomFunction[I, O, OP], OP] {
      override type In = I
      override type Out = O
    }

  implicit def debugOr[I, OP[_]]: Aux[Expr.Or[I, OP], OP, (I, Boolean, Boolean), Boolean] =
    new DebugArgs[Expr.Or[I, OP], OP] {
      override type In = (I, Boolean, Boolean)
      override type Out = Boolean
    }

  implicit def debugCombine[I, LI, LO, RI, RO, O, OP[_]](
    implicit
    evLOisLI: LO <:< LI,
    evROisRI: RO <:< RI,
  ): Aux[Expr.Combine[I, LI, LO, RI, RO, O, OP], OP, (I, LI, RI), O] =
    new DebugArgs[Expr.Combine[I, LI, LO, RI, RO, O, OP], OP] {
      override type In = (I, LI, RI)
      override type Out = O
    }

  implicit def debugConst[O, OP[_]]: Aux[Expr.Const[O, OP], OP, Any, O] =
    new DebugArgs[Expr.Const[O, OP], OP] {
      override type In = Any
      override type Out = O
    }

  implicit def debugIdent[I, OP[_]]: Aux[Expr.Identity[I, OP], OP, I, I] =
    new DebugArgs[Expr.Identity[I, OP], OP] {
      override type In = I
      override type Out = I
    }

  implicit def debugIsEqual[I, V, F[+_], OP[_]]: Aux[Expr.IsEqual[I, V, F, OP], OP, (I, F[V], F[V]), F[Boolean]] =
    new DebugArgs[Expr.IsEqual[I, V, F, OP], OP] {
      override type In = (I, F[V], F[V])
      override type Out = F[Boolean]
    }

  implicit def debugNot[I, O, OP[_]]: Aux[Expr.Not[I, O, OP], OP, (I, O), O] =
    new DebugArgs[Expr.Not[I, O, OP], OP] {
      override type In = (I, O)
      override type Out = O
    }

  implicit def debugExists[
    C[_],
    A,
    B,
    OP[_],
  ]: Aux[Expr.Exists[C, A, B, OP], OP, (C[A], Either[List[B], NonEmptyList[B]]), B] =
    new DebugArgs[Expr.Exists[C, A, B, OP], OP] {
      override type In = (C[A], Either[List[B], NonEmptyList[B]])
      override type Out = B
    }

  implicit def debugForAll[
    C[_],
    A,
    B,
    OP[_],
  ]: Aux[Expr.ForAll[C, A, B, OP], OP, (C[A], Either[NonEmptyList[B], List[B]]), B] =
    new DebugArgs[Expr.ForAll[C, A, B, OP], OP] {
      override type In = (C[A], Either[NonEmptyList[B], List[B]])
      override type Out = B
    }

  implicit def debugMapEvery[C[_], A, B, OP[_]]: Aux[Expr.MapEvery[C, A, B, OP], OP, C[A], C[B]] =
    new DebugArgs[Expr.MapEvery[C, A, B, OP], OP] {
      override type In = C[A]
      override type Out = C[B]
    }

  implicit def debugSelect[I, O, OP[_]]: Aux[Expr.Select[I, O, OP], OP, (I, VariantLens[I, O]), O] =
    new DebugArgs[Expr.Select[I, O, OP], OP] {
      override type In = (I, VariantLens[I, O])
      override type Out = O
    }

  implicit def debugValuesOfType[T, O, OP[_]]: Aux[Expr.ValuesOfType[T, O, OP], OP, Any, Seq[O]] =
    new DebugArgs[Expr.ValuesOfType[T, O, OP], OP] {
      override type In = Any
      override type Out = Seq[O]
    }

  implicit def debugWithinWindow[
    I,
    V,
    F[+_],
    OP[_],
  ]: Aux[Expr.WithinWindow[I, V, F, OP], OP, (I, F[V], F[Window[V]]), F[Boolean]] =
    new DebugArgs[Expr.WithinWindow[I, V, F, OP], OP] {
      override type In = (I, F[V], F[Window[V]])
      override type Out = F[Boolean]
    }

}
