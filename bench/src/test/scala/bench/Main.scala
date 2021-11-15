package com.rallyhealth.vapors

package bench

import bench.timeit.Benchmark

import java.util.concurrent.TimeUnit

object Main {

  def main(args: Array[String]): Unit = {
    val allBenchmarks: Seq[Benchmark] = Seq(
      SimpleWithCaching.benchmarkFindInSeqOfTagsLists,
      SimpleWithoutCaching.benchmarkFindInSeqOfTagsLists,
    )
    for (benchmark <- allBenchmarks) {
      val results = benchmark.run()
      println(results.display(TimeUnit.MICROSECONDS))
    }
  }
}
