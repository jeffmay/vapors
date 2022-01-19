package com.rallyhealth.vapors.v1

package time

import java.time.temporal.Temporal
import java.time._

trait CountTime[-T, -U, +O] {

  /**
    * Count the whole number of complete time units between the start and end.
    *
    * @param start the start of the time range (inclusive)
    * @param end the end of the time range (inclusive)
    * @param truncateToUnit the unit to round down to
    * @return the whole number of units between the start up to, but not including, the end
    */
  def between(
    start: T,
    end: T,
    truncateToUnit: U,
  ): O
}

object CountTime {

  def between[T, U, O](
    start: T,
    end: T,
    roundDownToUnit: U,
  )(implicit
    diffTime: CountTime[T, U, O],
  ): O = diffTime.between(start, end, roundDownToUnit)

  private final class OfTemporal[-T <: Temporal] extends CountTime[T, TemporalUnit, Long] {
    override def between(
      start: T,
      end: T,
      unit: TemporalUnit,
    ): Long = start.until(end, unit.chronoUnit)
  }

  @inline private def temporal[T <: Temporal]: CountTime[T, TemporalUnit, Long] = new OfTemporal[T]

  implicit val countInstant: CountTime[Instant, TimeUnit, Long] = temporal
  implicit val countLocalTime: CountTime[LocalTime, TimeUnit, Long] = temporal
  implicit val countOffsetTime: CountTime[OffsetTime, TimeUnit, Long] = temporal

  implicit val countLocalDate: CountTime[LocalDate, DateUnit, Long] = temporal

  implicit val countLocalDateTime: CountTime[LocalDateTime, TemporalUnit, Long] = temporal
  implicit val countOffsetDateTime: CountTime[OffsetDateTime, TemporalUnit, Long] = temporal
  implicit val countZonedDateTime: CountTime[ZonedDateTime, TemporalUnit, Long] = temporal
}
