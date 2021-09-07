package com.rallyhealth

package vapors.v1.data

import scala.annotation.implicitAmbiguous

/**
  * A value of type `HasInput[I, O]` provides implicit evidence that an [[vapors.v1.algebra.Expr]]
  * with input type `I` and output type `O` actually has input.
  *
  * In other words, it proves that `I` is not equal to `Nothing`.
  */
sealed abstract class HasInput[-I, -O]

object HasInput extends HasInput[Any, Any] {

  implicit def hasInput[I, O]: HasInput[I, O] = HasInput

  // Provide multiple ambiguous values so an implicit HasInput[Nothing] cannot be found.
  @implicitAmbiguous(
    "This ExprState[Nothing, ${O}] object has no input (hence the 'Nothing' on the left). This method cannot be called.",
  )
  implicit def hasInputNothing1[O]: HasInput[Nothing, O] = HasInput
  implicit def hasInputNothing2[O]: HasInput[Nothing, O] = HasInput
}
