package com.rallyhealth.vapors

package bench

import bench.timeit._
import v1.algebra.Expr

object ImmutableCachingBenchmarks extends FindInSeqOfTagsListsBenchmarkSetup {
  import v1.dsl.caching.immutable._

  import FindInSeqOfTagsListsBenchmarkSetup.Params

  override protected def findInSeqOfTagsLists(setup: VaporsBenchmarkSetup[Params, Boolean, OP]): Benchmark[Params] = {
    Benchmark.create(setup.config.prefixName("with caching: ")) {
      // Easier than doing a foldLeft, we can just rely on the implementation of Expr.Sequence to do the fold for us
      Expr.Sequence(setup.expressions).run(setup.factTable).foreach(setup.ensureValidResult)
    }
  }
}
