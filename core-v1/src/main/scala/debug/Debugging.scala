package com.rallyhealth.vapors.v1

package debug

import algebra.Expr
import data.ExprState

import scala.reflect.{classTag, ClassTag}

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
sealed abstract class Debugging[-I : ClassTag, -O : ClassTag] {

  def attach(state: ExprState[I, O]): Unit

  final def ignoreInvalidState: Debugging[Any, Any] = Debugging { state =>
    state.input match {
      case i: I =>
        state.output match {
          case o: O => attach(state.withBoth(i, o))
          case _ =>
        }
      case _ =>
    }
  }

  final def ignoreInvalidInput: Debugging[Any, O] = Debugging { state =>
    state.input match {
      case i: I => attach(state.withInput(i))
    }
  }

  final def ignoreInvalidOutput: Debugging[I, Any] = Debugging { state =>
    state.output match {
      case o: O => attach(state.withOutput(o))
    }
  }

  final def contramap[A : ClassTag, B : ClassTag](fn: ExprState[A, B] => ExprState[I, O]): Debugging[A, B] =
    Debugging { state =>
      attach(fn(state))
    }

  final def contramapInput[A : ClassTag](fn: A => I): Debugging[A, O] = Debugging { state =>
    attach(state.mapInput(fn))
  }

  final def contramapOutput[A : ClassTag](fn: A => O): Debugging[I, A] = Debugging { state =>
    attach(state.mapOutput(fn))
  }
}

object Debugging {

  def apply[I : ClassTag, O : ClassTag](hook: ExprState[I, O] => Unit): Debugging[I, O] =
    new Debugging[I, O] {
      override final def attach(state: ExprState[I, O]): Unit = hook(state)
      override final lazy val toString: String = s"Debugging[${classTag[I]}, ${classTag[O]}]"
    }

}

case object NoDebugging extends NoDebugging
sealed class NoDebugging private extends Debugging[Any, Any] {
  override def attach(state: ExprState[Any, Any]): Unit = {}
}
