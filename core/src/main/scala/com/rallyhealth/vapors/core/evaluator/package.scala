package com.rallyhealth.vapors.core

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg

package object evaluator {

  def eval[T, A](input: T)(exp: FreeApplicative[ExpAlg[T, *], A]): A = {
    exp.foldMap(new EvalLoop[T]).apply(input)
  }

}
