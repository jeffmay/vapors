package com.rallyhealth.vapors

package bench.timeit

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

final case class BenchmarkResults(
  nanoDurations: IndexedSeq[Long], // should this be non-empty?
) {
  lazy val durations: IndexedSeq[FiniteDuration] = nanoDurations.map(_.nanos)
  lazy val durationMin: FiniteDuration = nanoDurations.min.nanos
  lazy val durationMax: FiniteDuration = nanoDurations.max.nanos
  lazy val durationAvg: FiniteDuration = (nanoDurations.sum / nanoDurations.size).nanos
  lazy val durationStdDev: FiniteDuration = {
    if (nanoDurations.isEmpty)
      throw new UnsupportedOperationException("durationStdDev called on empty BenchmarkResults")

    val n = nanoDurations.length
    val u = nanoDurations.foldLeft(0d) {
      case (acc, nanos) =>
        acc + (nanos.toDouble / n)
    }
    val root = nanoDurations.foldLeft(0d) {
      case (acc, nanos) =>
        acc + (Math.pow(nanos.toDouble - u, 2) / n)
    }
    val stddevNanos = Math.pow(root, .5)
    stddevNanos.nanos
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
