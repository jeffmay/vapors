package com.rallyhealth.vapors.core.time

import java.time.{Duration, Instant, LocalDate, LocalDateTime, Period, ZonedDateTime}
import java.time.temporal.{Temporal, TemporalAmount}

trait ModifyTime[T, -D] {

  def addTo(
    temporal: T,
    amount: D,
  ): T
}

object ModifyTime {

  def addTo[T, D](
    temporal: T,
    amount: D,
  )(implicit
    modifier: ModifyTime[T, D],
  ): T = modifier.addTo(temporal, amount)

  private final class OfTemporal[T <: Temporal, -D <: TemporalAmount] extends ModifyTime[T, D] {
    override def addTo(
      temporal: T,
      amount: D,
    ): T = amount.addTo(temporal).asInstanceOf[T]
  }

  @inline private def temporal[T <: Temporal, D <: TemporalAmount]: ModifyTime[T, D] = new OfTemporal[T, D]

  implicit val ofInstant: ModifyTime[Instant, Duration] = temporal
  implicit val ofLocalDate: ModifyTime[LocalDate, Period] = temporal
  implicit val ofLocalDateTime: ModifyTime[LocalDateTime, TemporalAmount] = temporal
  implicit val ofZonedDateTime: ModifyTime[ZonedDateTime, TemporalAmount] = temporal
}
