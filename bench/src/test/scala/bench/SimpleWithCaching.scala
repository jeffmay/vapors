package com.rallyhealth.vapors

package bench

import bench.timeit._
import v1.engine.SimpleCachingEngine

object SimpleWithCaching extends FindInSeqOfTagsListsBenchmarkSetup {

  import v1.dsl.simple._

  override protected def findInSeqOfTagsLists(setup: VaporsBenchmarkSetup[Boolean, OP]): Benchmark = {
    Benchmark.create(BenchmarkConfig(s"with caching: ${setup.commonName}")) {
      SimpleCachingEngine.evalMultiple(setup.expressions, setup.factTable).foreach(setup.ensureValidResult)
    }
  }
}
