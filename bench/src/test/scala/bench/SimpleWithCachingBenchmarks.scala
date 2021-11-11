package com.rallyhealth.vapors

package bench

import bench.timeit._
import v1.engine.SimpleCachingEngine

object SimpleWithCachingBenchmarks extends FindInSeqOfTagsListsBenchmarkSetup {
  import FindInSeqOfTagsListsBenchmarkSetup.Params

  import v1.dsl.simple._

  override protected def findInSeqOfTagsLists(setup: VaporsBenchmarkSetup[Params, Boolean, OP]): Benchmark[Params] = {
    Benchmark.create(setup.config.prefixName("with caching: ")) {
      SimpleCachingEngine.evalMultiple(setup.expressions, setup.factTable).foreach(setup.ensureValidResult)
    }
  }
}
