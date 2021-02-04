package com.rallyhealth.vapors.factfilter.extras

import cats.Eval
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.TypedFact
import com.rallyhealth.vapors.factfilter.dsl.CaptureP
import com.rallyhealth.vapors.factfilter.evaluator.{ExprInput, ExprOutput}

object CaptureTimeRange extends CaptureP.AsMonoidCompanion[TimeRange] {

  implicit def captureTimeRangeFromFacts[T : ExtractInstant, R]: CaptureP.AsMonoidFromFactsOfType[T, R, TimeRange] = {
    new CaptureP.AsMonoidFromFactsOfType[T, R, TimeRange] {

      override protected def foldWithParentParam(
        expr: Expr[Seq, TypedFact[T], R, TimeRange],
        input: ExprInput[Seq, TypedFact[T]],
        output: ExprOutput[R],
        processedChildren: TimeRange,
      ): Eval[TimeRange] = {
        val timestamps = input.value.map(fact => ExtractInstant[T].extractValue(fact.value))
        Eval.now(TimeRange.fromIterable(timestamps))
      }
    }
  }
}
