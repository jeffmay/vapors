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

/**
  * Represents a unit that is a multiple of months.
  */
sealed abstract class MonthUnit(u: ChronoUnit) extends DateUnit(u)

/**
  * Represents a unit that is a multiple of years.
  */
sealed abstract class YearUnit(u: ChronoUnit) extends MonthUnit(u)

object TemporalUnit {

  case object Nanos extends TimeUnit(ChronoUnit.NANOS)
  case object Micros extends TimeUnit(ChronoUnit.MICROS)
  case object Millis extends TimeUnit(ChronoUnit.MILLIS)
  case object Seconds extends TimeUnit(ChronoUnit.SECONDS)
  case object Minutes extends TimeUnit(ChronoUnit.MINUTES)
  case object Hours extends TimeUnit(ChronoUnit.HOURS)

  case object Days extends DateUnit(ChronoUnit.DAYS)
  case object Weeks extends DateUnit(ChronoUnit.WEEKS)
  case object Months extends DateUnit(ChronoUnit.MONTHS)

  case object Years extends YearUnit(ChronoUnit.YEARS)
  case object Decades extends YearUnit(ChronoUnit.DECADES)
  case object Centuries extends YearUnit(ChronoUnit.CENTURIES)
  case object Millennia extends YearUnit(ChronoUnit.MILLENNIA)
}
