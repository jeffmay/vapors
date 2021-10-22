package com.rallyhealth.vapors.v1

package debug

import algebra.Expr
import data.ExprState

import izumi.reflect.Tag

import scala.annotation.nowarn
import scala.reflect.ClassTag

/**
  * Encapsulates the logic for attaching a debug hook.
  *
  * Since each [[Expr]] subclass has different input types for the debug hooks, we need [[ClassTag]]s
  * to make sure that our runtime types match the expected types as laid out by the implicit
  * [[com.rallyhealth.vapors.v1.debug.DebugArgs]] for the specific Expr subclass.
  *
  * TODO: The ability to alter the local state might also be helpful for debugging purposes, but it should
  *       be something that you can disable for a more "production secure" setup. That said... maybe the
  *       entire capability of attaching a side-effecting function should be something you can turn off.
  *       Either way, we should always support this `ExprState => Unit` interface, so it would have to be
  *       a separate method anyway.
  */
sealed abstract class Debugging[-I : ClassTag : Tag, -O : ClassTag : Tag] {

  def attach(state: ExprState[I, O]): Unit

  final def throwOnInvalidState: Debugging[Any, Any] = Debugging { state =>
    import Debugging.{input, output}
    def correct[V : Tag](
      position: String,
      @nowarn observed: V,
    ): String = s"the correct type of $position (${Tag[V].tag})"
    def incorrect[V : Tag](
      position: String,
      observed: Any,
    ): String = observed match {
      case ExprState.Nothing => s"no $position (i.e. ExprState.Nothing) (expected ${Tag[V].tag})"
      case _ => s"the incorrect type of $position (${observed.getClass.getName}) (expected ${Tag[V].tag})"
    }
    def fail(
      inputMsg: String,
      outputMsg: String,
    ): Nothing = {
      val msg = s"$this provided $inputMsg and $outputMsg"
      throw new IllegalStateException(msg)
    }
    state.input match {
      case i: I =>
        state.output match {
          case o: O => attach(state.withBoth(i, o))
          case o => fail(correct[I](input, i), incorrect[O](output, o))
        }
      case i =>
        state.output match {
          case o: O => fail(incorrect[I](input, i), correct[O](output, o))
          case o => fail(incorrect[I](input, i), incorrect[O](output, o))
        }
    }
  }

  final def contramap[A : ClassTag : Tag, B : ClassTag : Tag](fn: ExprState[A, B] => ExprState[I, O]): Debugging[A, B] =
    Debugging { state =>
      attach(fn(state))
    }

  final def contramapInput[A : ClassTag : Tag](fn: A => I): Debugging[A, O] = Debugging { state =>
    attach(state.mapInput(fn))
  }

  final def contramapOutput[A : ClassTag : Tag](fn: A => O): Debugging[I, A] = Debugging { state =>
    attach(state.mapOutput(fn))
  }

  override lazy val toString: String = {
    s"Debugging[${Tag[I].tag}, ${Tag[O].tag}]"
  }
}

object Debugging {

  private final val input = "input"
  private final val output = "output"

  def apply[I : ClassTag : Tag, O : ClassTag : Tag](hook: ExprState[I, O] => Unit): Debugging[I, O] =
    new Debugging[I, O] {
      override final def attach(state: ExprState[I, O]): Unit = hook(state)
    }

}

case object NoDebugging extends NoDebugging
sealed class NoDebugging private extends Debugging[Any, Any] {
  override final def attach(state: ExprState[Any, Any]): Unit = {}
  override final lazy val toString: String = "NoDebugging"
}
