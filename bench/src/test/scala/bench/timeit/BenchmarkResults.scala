package com.rallyhealth.vapors

package bench.timeit

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

final case class BenchmarkResults(
  config: BenchmarkConfig,
  nanoDurations: IndexedSeq[Long],
) {
  lazy val durations: IndexedSeq[FiniteDuration] = nanoDurations.map(_.nanos)
  lazy val durationMin: FiniteDuration = nanoDurations.min.nanos
  lazy val durationMax: FiniteDuration = nanoDurations.max.nanos
  lazy val durationAvg: FiniteDuration = (nanoDurations.sum / nanoDurations.size).nanos

  def display(unit: TimeUnit = TimeUnit.MICROSECONDS): String = {
    val u = BenchmarkResults.shortUnitName(unit)
    s"""BENCHMARK: ${config.name}
       |Avg: ${durationAvg.toUnit(unit)}$u, Min: ${durationMin.toUnit(unit)}$u, Max: ${durationMax.toUnit(unit)}$u
       |""".stripMargin
  }
}

object BenchmarkResults {

  def shortUnitName(unit: TimeUnit): String = unit match {
    case TimeUnit.NANOSECONDS => "ns"
    case TimeUnit.MICROSECONDS => "µs"
    case TimeUnit.MILLISECONDS => "ms"
    case TimeUnit.SECONDS => "s"
    case TimeUnit.MINUTES => "m"
    case TimeUnit.HOURS => "h"
    case TimeUnit.DAYS => "d"
  }
}
