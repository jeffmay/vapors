package com.rallyhealth.vapors

package bench.timeit

final class Benchmark[+P] private (
  val config: BenchmarkConfig[P],
  runFn: () => BenchmarkResults,
) {
  def run(): BenchmarkResults = runFn()
}

object Benchmark {

  def create[P](config: BenchmarkConfig[P])(block: => Any): Benchmark[P] =
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
      BenchmarkResults(resultArray)
    })
}
