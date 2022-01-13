package com.rallyhealth.vapors

package bench

import java.util.concurrent.TimeUnit

object Main {

  def main(args: Array[String]): Unit = {
    val benchmarkMatrix =
      ImmutableCachingBenchmarks.benchmarkMultiFindInSeqOfTags ++ UncachedBenchmarks.benchmarkMultiFindInSeqOfTags
    println("Name,Exprs,Facts,Tags/Fact,Avg,StdDev")
    val unit = TimeUnit.MICROSECONDS
    for (benchmark <- benchmarkMatrix) {
      val results = benchmark.run()
      val p = benchmark.config.params
      val row = (
        benchmark.config.name.takeWhile(_ != ':'),
        p.numExpressions,
        p.numTagFacts,
        p.numTagsPerFact,
        results.durationAvg.toUnit(unit),
        results.durationStdDev.toUnit(unit),
      )
      println(row.productIterator.mkString(","))
    }
  }
}
