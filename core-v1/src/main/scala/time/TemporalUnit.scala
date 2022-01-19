package com.rallyhealth.vapors.v1

package time

import java.time.temporal.ChronoUnit

/**
  * A hierarchically-typed version of [[ChronoUnit]] that distinguishes between [[DateUnit]]s and [[TimeUnit]]s.
  */
sealed abstract class TemporalUnit(val chronoUnit: ChronoUnit)

/**
  * Represents a unit of time within a day.
  */
sealed abstract class TimeUnit(u: ChronoUnit) extends TemporalUnit(u)

/**
  * Represents a unit that is a multiple of days.
  */
sealed abstract class DateUnit(u: ChronoUnit) extends TemporalUnit(u)

object TemporalUnit {

  final case object Nanos extends TimeUnit(ChronoUnit.NANOS)
  final case object Micros extends TimeUnit(ChronoUnit.MICROS)
  final case object Millis extends TimeUnit(ChronoUnit.MILLIS)
  final case object Seconds extends TimeUnit(ChronoUnit.SECONDS)
  final case object Minutes extends TimeUnit(ChronoUnit.MINUTES)
  final case object Hours extends TimeUnit(ChronoUnit.HOURS)

  final case object Days extends DateUnit(ChronoUnit.DAYS)
  final case object Weeks extends DateUnit(ChronoUnit.WEEKS)
  final case object Months extends DateUnit(ChronoUnit.MONTHS)
  final case object Years extends DateUnit(ChronoUnit.YEARS)
  final case object Decades extends DateUnit(ChronoUnit.DECADES)
  final case object Centuries extends DateUnit(ChronoUnit.CENTURIES)
  final case object Millennia extends DateUnit(ChronoUnit.MILLENNIA)
}
