package com.rallyhealth.vapors.factfilter

import com.rallyhealth.vapors.core.interpreter

package object evaluator {

  @deprecated("Use com.rallyhealth.vapors.core.interpreter.ExprInput instead.", "0.8.0")
  final type ExprInput[F[_], V] = interpreter.ExprInput[F, V]

  @deprecated("Use com.rallyhealth.vapors.core.interpreter.ExprInput instead.", "0.8.0")
  final val ExprInput = interpreter.ExprInput

  @deprecated("Use com.rallyhealth.vapors.core.interpreter.ExprOutput instead.", "0.8.0")
  final type ExprOutput[R] = interpreter.ExprOutput[R]

  @deprecated("Use com.rallyhealth.vapors.core.interpreter.ExprOutput instead.", "0.8.0")
  final val ExprOutput = interpreter.ExprOutput

  @deprecated("Use com.rallyhealth.vapors.core.interpreter.InterpretExprAsResultFn instead.", "0.8.0")
  final type InterpretExprAsResultFn[F[_], V, P] = interpreter.InterpretExprAsResultFn[F, V, P]

  @deprecated("Use com.rallyhealth.vapors.core.interpreter.InterpretExprAsResultFn instead.", "0.8.0")
  final val InterpretExprAsResultFn = interpreter.InterpretExprAsResultFn

  @deprecated("Use com.rallyhealth.vapors.core.interpreter.InterpretExprAsResultFn instead.", "0.8.0")
  final type InterpretExprAsSimpleOutputFn[F[_], V, P] = interpreter.InterpretExprAsSimpleOutputFn[F, V, P]

  @deprecated("Use com.rallyhealth.vapors.core.interpreter.InterpretExprAsResultFn instead.", "0.8.0")
  final val InterpretExprAsSimpleOutputFn = interpreter.InterpretExprAsSimpleOutputFn
}
