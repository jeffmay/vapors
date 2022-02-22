package com.rallyhealth.vapors.v1

package debug

import algebra.{Expr, SizeComparison}
import data._
import lens.VariantLens

import cats.Eval
import cats.data.{NonEmptySeq, NonEmptyVector}
import com.rallyhealth.vapors.v1.algebra.Expr.MatchCase
import izumi.reflect.Tag
import shapeless.{unexpected, HList}

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

  implicit def debugAndThen[II, IO, OI, OO, OP[_]]: Aux[Expr.AndThen[II, IO, OI, OO, OP], OP, (II, IO), OO] =
    new DebugArgs[Expr.AndThen[II, IO, OI, OO, OP], OP] {
      override type In = (II, IO)
      override type Out = OO
    }

  implicit def debugCustomFunction[I, O, OP[_]]: Aux[Expr.CustomFunction[I, O, OP], OP, I, O] =
    new DebugArgs[Expr.CustomFunction[I, O, OP], OP] {
      override type In = I
      override type Out = O
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

  implicit def debugAnd[I, B, W[+_], OP[_]]: Aux[Expr.And[I, B, W, OP], OP, (I, NonEmptyVector[W[B]]), W[B]] =
    new DebugArgs[Expr.And[I, B, W, OP], OP] {
      override type In = (I, NonEmptyVector[W[B]])
      override type Out = W[B]
    }

  implicit def debugOr[I, B, W[+_], OP[_]]: Aux[Expr.Or[I, B, W, OP], OP, (I, NonEmptyVector[W[B]]), W[B]] =
    new DebugArgs[Expr.Or[I, B, W, OP], OP] {
      override type In = (I, NonEmptyVector[W[B]])
      override type Out = W[B]
    }

  implicit def debugNot[I, O, W[+_], OP[_]]: Aux[Expr.Not[I, O, W, OP], OP, (I, W[O]), W[O]] =
    new DebugArgs[Expr.Not[I, O, W, OP], OP] {
      override type In = (I, W[O])
      override type Out = W[O]
    }

  implicit def debugConst[O, OP[_]]: Aux[Expr.Const[O, OP], OP, Any, O] =
    new DebugArgs[Expr.Const[O, OP], OP] {
      override type In = Any
      override type Out = O
    }

  implicit def debugConvert[I, O, OP[_]]: Aux[Expr.Convert[I, O, OP], OP, I, O] =
    new DebugArgs[Expr.Convert[I, O, OP], OP] {
      override type In = I
      override type Out = O
    }

  implicit def debugContainsAny[
    I,
    W[+_],
    C[_],
    A,
    B,
    OP[_],
  ]: Aux[Expr.ContainsAny[I, W, C, A, B, OP], OP, (I, C[W[A]], Set[W[A]], Seq[W[A]]), B] =
    new DebugArgs[Expr.ContainsAny[I, W, C, A, B, OP], OP] {
      override type In = (I, C[W[A]], Set[W[A]], Seq[W[A]])
      override type Out = B
    }

  implicit def debugDefine[I, C[_], T, OP[_]]: Aux[Expr.Define[I, C, T, OP], OP, (I, C[T]), Seq[TypedFact[T]]] =
    new DebugArgs[Expr.Define[I, C, T, OP], OP] {
      override type In = (I, C[T])
      override type Out = Seq[TypedFact[T]]
    }

  implicit def debugIdent[I, OP[_]]: Aux[Expr.Identity[I, OP], OP, I, I] =
    new DebugArgs[Expr.Identity[I, OP], OP] {
      override type In = I
      override type Out = I
    }

  implicit def debugIsEqual[I, V, W[+_], OP[_]]: Aux[Expr.IsEqual[I, V, W, OP], OP, (I, W[V], W[V]), W[Boolean]] =
    new DebugArgs[Expr.IsEqual[I, V, W, OP], OP] {
      override type In = (I, W[V], W[V])
      override type Out = W[Boolean]
    }

  implicit def debugGetOrElse[I, O, OP[_]]: Aux[Expr.GetOrElse[I, O, OP], OP, (I, Option[O]), O] =
    new DebugArgs[Expr.GetOrElse[I, O, OP], OP] {
      override type In = (I, Option[O])
      override type Out = O
    }

  implicit def debugExists[
    C[_],
    A,
    B,
    OP[_],
  ]: Aux[Expr.Exists[C, A, B, OP], OP, (C[A], Either[Seq[B], NonEmptySeq[B]]), B] =
    new DebugArgs[Expr.Exists[C, A, B, OP], OP] {
      override type In = (C[A], Either[Seq[B], NonEmptySeq[B]])
      override type Out = B
    }

  implicit def debugFilter[C[_], A, B, D[_], OP[_]]: Aux[Expr.Filter[C, A, B, D, OP], OP, C[A], D[A]] =
    new DebugArgs[Expr.Filter[C, A, B, D, OP], OP] {
      override type In = C[A]
      override type Out = D[A]
    }

  implicit def debugFlatten[C[_], A, OP[_]]: Aux[Expr.Flatten[C, A, OP], OP, C[C[A]], C[A]] =
    new DebugArgs[Expr.Flatten[C, A, OP], OP] {
      override type In = C[C[A]]
      override type Out = C[A]
    }

  implicit def debugFoldLeft[I, C[_], A, O, OP[_]]: Aux[Expr.FoldLeft[I, C, A, O, OP], OP, (I, C[A], O), O] =
    new DebugArgs[Expr.FoldLeft[I, C, A, O, OP], OP] {
      override type In = (I, C[A], O)
      override type Out = O
    }

  implicit def debugForAll[
    C[_],
    A,
    B,
    OP[_],
  ]: Aux[Expr.ForAll[C, A, B, OP], OP, (C[A], Either[NonEmptySeq[B], Seq[B]]), B] =
    new DebugArgs[Expr.ForAll[C, A, B, OP], OP] {
      override type In = (C[A], Either[NonEmptySeq[B], Seq[B]])
      override type Out = B
    }

  implicit def debugMapEvery[C[_], A, B, OP[_]]: Aux[Expr.MapEvery[C, A, B, OP], OP, C[A], C[B]] =
    new DebugArgs[Expr.MapEvery[C, A, B, OP], OP] {
      override type In = C[A]
      override type Out = C[B]
    }

  implicit def debugRepeat[I, O, OP[_]]: Aux[Expr.Repeat[I, O, OP], OP, (I, Eval[O], Option[Int]), IterableOnce[O]] =
    new DebugArgs[Expr.Repeat[I, O, OP], OP] {
      override type In = (I, Eval[O], Option[Int])
      override type Out = IterableOnce[O]
    }

  implicit def debugSelect[I, A, B, O, OP[_]]: Aux[Expr.Select[I, A, B, O, OP], OP, (I, A, VariantLens[A, B], B), O] =
    new DebugArgs[Expr.Select[I, A, B, O, OP], OP] {
      override type In = (I, A, VariantLens[A, B], B)
      override type Out = O
    }

  implicit def debugSequence[C[+_], I, O, OP[_]]: Aux[Expr.Sequence[C, I, O, OP], OP, I, C[O]] =
    new DebugArgs[Expr.Sequence[C, I, O, OP], OP] {
      override type In = I
      override type Out = C[O]
    }

  implicit def debugSizeIs[I, N, B, OP[_]]: Aux[Expr.SizeIs[I, N, B, OP], OP, (I, SizeComparison, N), B] =
    new DebugArgs[Expr.SizeIs[I, N, B, OP], OP] {
      override type In = (I, SizeComparison, N)
      override type Out = B
    }

  implicit def debugSlice[C[_], A, D[_], OP[_]]: Aux[Expr.Slice[C, A, D, OP], OP, (C[A], SliceRange.Absolute), D[A]] =
    new DebugArgs[Expr.Slice[C, A, D, OP], OP] {
      override type In = (C[A], SliceRange.Absolute)
      override type Out = D[A]
    }

  implicit def debugSorted[C[_], A, OP[_]]: Aux[Expr.Sorted[C, A, OP], OP, C[A], C[A]] =
    new DebugArgs[Expr.Sorted[C, A, OP], OP] {
      override type In = C[A]
      override type Out = C[A]
    }

  implicit def debugUsingDefinitions[I, O, OP[_]]: Aux[Expr.UsingDefinitions[I, O, OP], OP, (I, FactSet), O] =
    new DebugArgs[Expr.UsingDefinitions[I, O, OP], OP] {
      override type In = (I, FactSet)
      override type Out = O
    }

  implicit def debugValuesOfType[T, O, OP[_]]: Aux[Expr.ValuesOfType[T, O, OP], OP, Any, Seq[O]] =
    new DebugArgs[Expr.ValuesOfType[T, O, OP], OP] {
      override type In = Any
      override type Out = Seq[O]
    }

  implicit def debugMatch[I, S, B, O, OP[_]]: Aux[Expr.Match[I, S, B, O, OP], OP, (I, Option[Int]), Option[O]] =
    new DebugArgs[Expr.Match[I, S, B, O, OP], OP] {
      override type In = (I, Option[Int])
      override type Out = Option[O]
    }

  implicit def debugWhen[I, B, O, OP[_]]: Aux[Expr.When[I, B, O, OP], OP, (I, Int), O] =
    new DebugArgs[Expr.When[I, B, O, OP], OP] {
      override type In = (I, Int)
      override type Out = O
    }

  implicit def debugWithinWindow[
    I,
    V,
    W[+_],
    OP[_],
  ]: Aux[Expr.WithinWindow[I, V, W, OP], OP, (I, W[V], W[Window[V]]), W[Boolean]] =
    new DebugArgs[Expr.WithinWindow[I, V, W, OP], OP] {
      override type In = (I, W[V], W[Window[V]])
      override type Out = W[Boolean]
    }

  implicit def debugToHList[I, L <: HList, OP[_]]: Aux[Expr.ToHList[I, L, OP], OP, I, L] =
    new DebugArgs[Expr.ToHList[I, L, OP], OP] {
      override type In = I
      override type Out = L
    }

  implicit def debugZipToShortestHList[
    I,
    F[+_],
    WL <: HList,
    UL <: HList,
    OP[_],
  ]: Aux[Expr.ZipToShortestHList[I, F, WL, UL, OP], OP, I, F[UL]] =
    new DebugArgs[Expr.ZipToShortestHList[I, F, WL, UL, OP], OP] {
      override type In = I
      override type Out = F[UL]
    }

}
