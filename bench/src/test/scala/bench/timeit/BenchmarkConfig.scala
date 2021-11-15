package com.rallyhealth.vapors

package bench.timeit

final case class BenchmarkConfig(
  name: String,
  iterations: Int = BenchmarkConfig.defaultIterations,
  warmupIterations: Int = BenchmarkConfig.defaultWarmupIterations,
)

object BenchmarkConfig {
  // TODO: Read defaults from a config file / env vars / properties?
  final val defaultIterations = 10_000
  final val defaultWarmupIterations = 5_000
}
