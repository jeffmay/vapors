package com.rallyhealth.vapors

package bench

import bench.timeit.BenchmarkConfig
import v1.algebra.Expr
import v1.data.FactTable

// TODO: Use type member?
final class VaporsBenchmarkSetup[+P, R, OP[_]] private (
  val config: BenchmarkConfig[P],
  val factTable: FactTable,
  val expressions: Seq[Expr[Any, R, OP]],
  validateResult: R => Boolean,
) {

  def ensureValidResult(result: R): Unit = {
    require(validateResult(result), s"Invalid result $result for benchmark: '${config.name}''")
  }
}

object VaporsBenchmarkSetup {

  def apply[P](
    config: BenchmarkConfig[P],
    factTable: FactTable,
  ): RepeatBuilder[P] =
    new RepeatBuilder(config, factTable)

  final class RepeatBuilder[P](
    config: BenchmarkConfig[P],
    factTable: FactTable,
  ) {

    def single[R, OP[_]](expr: Expr[Any, R, OP]): ValidatingBuilder[P, R, OP] =
      repeated(1)(expr)

    def repeated[R, OP[_]](numRepeats: Int)(expr: Expr[Any, R, OP]): ValidatingBuilder[P, R, OP] =
      new ValidatingBuilder(config, factTable, Seq.fill(numRepeats)(expr))
  }

  final class ValidatingBuilder[P, R, OP[_]](
    config: BenchmarkConfig[P],
    factTable: FactTable,
    expressions: Seq[Expr[Any, R, OP]],
  ) {

    def ensuringResultIsTrue(implicit ev: R =:= Boolean): VaporsBenchmarkSetup[P, R, OP] =
      ensuringResult(ev)

    def ensuringResult(cond: R => Boolean): VaporsBenchmarkSetup[P, R, OP] =
      new VaporsBenchmarkSetup(config, factTable, expressions, cond)

    def ignoreResult: VaporsBenchmarkSetup[P, R, OP] = ensuringResult(_ => true)
  }
}
