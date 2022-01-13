package com.rallyhealth.vapors.v1

package dsl

import algebra.{CombineHolder, SelectHolder}
import data.FactTable

trait UnwrappedRunDsl extends StatelessRunDsl {
  self: DslTypes =>

  override final type RunResult[+O] = O
  override final type RunWithResult[+PO, -I, +O] = O

  override implicit def runAny[O](expr: Any ~:> O): RunUnwrappedExpr[O] = new RunUnwrappedExpr(expr)

  override implicit def runWith[I, O](expr: I ~:> O): RunUnwrappedWithInputExpr[I, O] =
    new RunUnwrappedWithInputExpr(expr)

  override implicit def runCombine[O : OP](
    builder: CombineHolder[Any, Nothing, Any, Nothing, Any, O, OP],
  ): RunUnwrappedExpr[O] = new RunUnwrappedExpr(builder.toExpr)

  override implicit def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): RunUnwrappedWithInputExpr[I, O] = new RunUnwrappedWithInputExpr(builder.toExpr)

  override implicit def runSelect[A, B, O : OP](builder: SelectHolder[Any, A, B, O, OP]): RunUnwrappedExpr[O] =
    new RunUnwrappedExpr(builder.toExpr)

  override implicit def runSelectWith[I, A, B, O : OP](
    builder: SelectHolder[I, A, B, O, OP],
  ): RunUnwrappedWithInputExpr[I, O] =
    new RunUnwrappedWithInputExpr(builder.toExpr)

  class RunUnwrappedExpr[+O](expr: Any ~:> O) extends RunExpr[O](expr) {
    override def run(): O = super.run()
    override def run(factTable: FactTable): O = super.run(factTable)
  }

  class RunUnwrappedWithInputExpr[-I, +O](expr: I ~:> O) extends RunWithInputExpr[I, O](expr) {
    override def runWith[In <: I](input: In): O = super.runWith(input)
    override def runWith[In <: I](
      input: In,
      factTable: FactTable,
    ): O = super.runWith(input, factTable)
  }
}
