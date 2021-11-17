package com.rallyhealth.vapors

package bench.timeit

final case class BenchmarkConfig[+P](
  name: String,
  params: P,
  iterations: Int = BenchmarkConfig.defaultIterations,
  warmupIterations: Int = BenchmarkConfig.defaultWarmupIterations,
) {

  def prefixName(prefix: String): BenchmarkConfig[P] =
    copy(name = prefix + this.name)
}

object BenchmarkConfig {
  // TODO: Read defaults from a config file / env vars / properties?
  final val defaultIterations = 10_000
  final val defaultWarmupIterations = 5_000
}
