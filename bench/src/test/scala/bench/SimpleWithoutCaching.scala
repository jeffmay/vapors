package com.rallyhealth.vapors

package bench

import bench.timeit._

object SimpleWithoutCaching extends FindInSeqOfTagsListsBenchmarkSetup {
  import FindInSeqOfTagsListsBenchmarkSetup.Params

  import v1.dsl.simple._

  override protected def findInSeqOfTagsLists(setup: VaporsBenchmarkSetup[Params, Boolean, OP]): Benchmark[Params] = {
    Benchmark.create(setup.config.prefixName("without caching: ")) {
      setup.expressions.foreach { e =>
        setup.ensureValidResult(e.run(setup.factTable))
      }
    }
  }
}
