package com.rallyhealth.vapors

package bench

import bench.timeit._

object SimpleWithoutCaching extends FindInSeqOfTagsListsBenchmarkSetup {

  import v1.dsl.simple._

  override protected def findInSeqOfTagsLists(setup: VaporsBenchmarkSetup[Boolean, OP]): Benchmark = {
    Benchmark.create(BenchmarkConfig(s"without caching: ${setup.commonName}")) {
      setup.expressions.foreach { e =>
        setup.ensureValidResult(e.run(setup.factTable))
      }
    }
  }
}
