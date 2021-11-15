package com.rallyhealth.vapors

package bench

import v1.algebra.Expr
import v1.data.FactTable

// TODO: Use type member?
final class VaporsBenchmarkSetup[R, OP[_]] private (
  val commonName: String,
  val factTable: FactTable,
  val expressions: Seq[Expr[Any, R, OP]],
  validateResult: R => Boolean,
) {

  def ensureValidResult(result: R): Unit = {
    require(validateResult(result), s"Invalid result $result for benchmark: '$commonName''")
  }
}

object VaporsBenchmarkSetup {

  def apply(
    commonName: String,
    factTable: FactTable,
  ): RepeatBuilder =
    new RepeatBuilder(commonName, factTable)

  final class RepeatBuilder(
    commonName: String,
    factTable: FactTable,
  ) {

    def single[R, OP[_]](expr: Expr[Any, R, OP]): ValidatingBuilder[R, OP] =
      repeated(1)(expr)

    def repeated[R, OP[_]](numRepeats: Int)(expr: Expr[Any, R, OP]): ValidatingBuilder[R, OP] =
      new ValidatingBuilder(commonName, factTable, Seq.fill(numRepeats)(expr))
  }

  final class ValidatingBuilder[R, OP[_]](
    commonName: String,
    factTable: FactTable,
    expressions: Seq[Expr[Any, R, OP]],
  ) {

    def ensuringResultIsTrue(implicit ev: R =:= Boolean): VaporsBenchmarkSetup[R, OP] =
      ensuringResult(ev)

    def ensuringResult(cond: R => Boolean): VaporsBenchmarkSetup[R, OP] =
      new VaporsBenchmarkSetup(commonName, factTable, expressions, cond)

    def ignoreResult: VaporsBenchmarkSetup[R, OP] = ensuringResult(_ => true)
  }
}
