package com.rallyhealth.vapors

package bench

import bench.timeit._
import v1.data.ExprState
import v1.engine.SimpleEngine

object UncachedBenchmarks extends FindInSeqOfTagsListsBenchmarkSetup {
  import v1.dsl.uncached._

  import FindInSeqOfTagsListsBenchmarkSetup.Params

  override protected def findInSeqOfTagsLists(setup: VaporsBenchmarkSetup[Params, Boolean, OP]): Benchmark[Params] = {
    Benchmark.create(setup.config.prefixName("without caching: ")) {
      setup.expressions.foreach { e =>
        val result = e.visit(SimpleEngine[OP](setup.factTable))(ExprState.Empty(setup.factTable))
        setup.ensureValidResult(result)
      }
    }
  }
}
