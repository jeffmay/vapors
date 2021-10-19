package com.rallyhealth.vapors.v1

package data

final case class ExprState[+I, +O](
  factTable: FactTable,
  private val maybeInput: Option[I],
  private val maybeOutput: Option[O],
) {

  def input(implicit hasInput: HasInput[I, O]): I = maybeInput.getOrElse(ExprState.Nothing.asInstanceOf[I])
  def output(implicit hasOutput: HasOutput[I, O]): O = maybeOutput.getOrElse(ExprState.Nothing.asInstanceOf[O])

  def withFactTable(factTable: FactTable): ExprState[I, O] = copy(factTable = factTable)

  def bimap[A, B](
    mapInput: I => A,
    mapOutput: O => B,
  ): ExprState[A, B] = copy(maybeInput = maybeInput.map(mapInput), maybeOutput = maybeOutput.map(mapOutput))

  def withBoth[A, B](
    input: A,
    output: B,
  ): ExprState[A, B] = copy(maybeInput = Some(input), maybeOutput = Some(output))

  def flatMapOutput[A](fn: O => Option[A]): ExprState[I, A] = copy(maybeOutput = maybeOutput.flatMap(fn))
  def mapOutput[A](fn: O => A): ExprState[I, A] = copy(maybeOutput = maybeOutput.map(fn))
  def withOutput[A](output: A): ExprState[I, A] = copy(maybeOutput = Some(output))
  def withNoOutput: ExprState[I, Nothing] = copy(maybeOutput = None)

  def flatMapInput[A](fn: I => Option[A]): ExprState[A, O] = copy(maybeInput = maybeInput.flatMap(fn))
  def mapInput[A](fn: I => A): ExprState[A, O] = copy(maybeInput = maybeInput.map(fn))
  def withInput[A](input: A): ExprState[A, O] = copy(maybeInput = Some(input))
  def withNoInput: ExprState[Nothing, O] = copy(maybeInput = None)

  /**
    * A helpful method for simultaneously shifting the output of this expression to become the input
    * of the next expression state while also setting the output of that expression.
    */
  def swapAndReplaceOutput[A](output: A): ExprState[O, A] =
    copy(maybeInput = maybeOutput, maybeOutput = Some(output))
}

object ExprState {

  /**
    * Applies the same implicit conversion logic provided by [[<:<.refl]], but for the entire [[ExprState]] object.
    *
    * Requires evidence that either or both of the known input and output types are subtypes (or the same type)
    * as the desired input and output types, respectively.
    */
  implicit def conforms[AI, AO, BI, BO](
    state: ExprState[AI, AO],
  )(implicit
    evBI: AI <:< BI,
    evBO: AO <:< BO,
  ): ExprState[BI, BO] = state.bimap(evBI, evBO)

  type Empty = ExprState[Nothing, Nothing]
  def Empty(factTable: FactTable): Empty = ExprState[Nothing, Nothing](factTable, None, None)
  final val Empty: Empty = Empty(FactTable.empty)

  /** A sentinel value for empty ExprState input or output */
  final case object Nothing

  type Input[+I] = ExprState[I, Nothing]

  object Input {

    def apply[I](
      input: I,
      factTable: FactTable,
    ): ExprState[I, Nothing] =
      ExprState(factTable, Some(input), None)
  }

  type Output[+O] = ExprState[Nothing, O]

  object Output {

    def apply[O](
      output: O,
      factTable: FactTable,
    ): ExprState[Nothing, O] =
      ExprState(factTable, None, Some(output))
  }
}
