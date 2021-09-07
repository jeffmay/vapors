package com.rallyhealth

package vapors.v1.data

import scala.annotation.implicitAmbiguous

/**
  * A value of type `HasOutput[I, O]` provides implicit evidence that an [[vapors.v1.algebra.Expr]]
  * with input type `I` and output type `O` actually has output.
  *
  * In other words, it proves that `O` is not equal to `Nothing`.
  */
sealed abstract class HasOutput[-I, -O]

object HasOutput extends HasOutput[Any, Any] {

  implicit def hasOutput[I, O]: HasOutput[I, O] = HasOutput

  // Provide multiple ambiguous values so an implicit HasInput[Nothing] cannot be found.
  @implicitAmbiguous(
    "This ExprState[${I}, Nothing] object has no output yet (hence the 'Nothing' on the right). This method cannot be called.",
  )
  implicit def hasOutputAmbiguous1[I]: HasOutput[I, Nothing] = HasOutput
  implicit def hasOutputAmbiguous2[I]: HasOutput[I, Nothing] = HasOutput
}
