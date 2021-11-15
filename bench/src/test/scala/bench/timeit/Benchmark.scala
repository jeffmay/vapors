package com.rallyhealth.vapors

package bench.timeit

final class Benchmark private (
  val config: BenchmarkConfig,
  runFn: () => BenchmarkResults,
) {
  def run(): BenchmarkResults = runFn()
}

object Benchmark {

  def create(config: BenchmarkConfig)(block: => Any): Benchmark =
    new Benchmark(config, () => {
      for (_ <- 0 until config.warmupIterations) {
        block
      }
      val resultArray = new Array[Long](config.iterations)
      for (i <- 0 until config.iterations) {
        val start = System.nanoTime()
        block
        val end = System.nanoTime()
        resultArray(i) = end - start
      }
      BenchmarkResults(config, resultArray)
    })
}
