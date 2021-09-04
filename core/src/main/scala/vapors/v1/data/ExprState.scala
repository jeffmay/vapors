package com.rallyhealth

package vapors.v1.data

// TODO: Figure out how to incorporate justification
final case class ExprState[+I, +O](
  factTable: FactTable,
  private val maybeInput: Option[I],
  private val maybeOutput: Option[O],
) {

  def input(implicit hasInput: HasInput[I, O]): I = maybeInput.get
  def output(implicit hasOutput: HasOutput[I, O]): O = maybeOutput.get

  def withFactTable(factTable: FactTable): ExprState[I, O] = copy(factTable = factTable)
  def withOutput[A](output: A): ExprState[I, A] = copy(maybeOutput = Some(output))
  def withInput[A](input: A): ExprState[A, O] = copy(maybeInput = Some(input))

  def swapAndReplaceOutput[A](output: A): ExprState[O, A] =
    copy(maybeInput = maybeOutput, maybeOutput = Some(output))
}

object ExprState {

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