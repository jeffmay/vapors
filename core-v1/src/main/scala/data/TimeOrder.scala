package com.rallyhealth.vapors.v1

package data

import cats.syntax.contravariant._
import cats.Order

import java.time._
import java.time.chrono.{ChronoLocalDate, ChronoLocalDateTime, ChronoZonedDateTime}

class TimeOrder {

  protected def modify[T](order: Order[T]): Order[T] = order

  @deprecated("Use orderLocalDate instead.", "1.0.0")
  def latestLocalDateFirst: Order[LocalDate] = orderLocalDate

  implicit val orderLocalDate: Order[LocalDate] = modify {
    Order.fromComparable[ChronoLocalDate].contramap(identity[ChronoLocalDate])
  }

  @deprecated("Use orderLocalDate instead.", "1.0.0")
  def latestLocalDateTimeFirst: Order[LocalDateTime] = orderLocalDateTime

  implicit val orderLocalDateTime: Order[LocalDateTime] = modify {
    Order
      .fromComparable[ChronoLocalDateTime[_ <: ChronoLocalDate]]
      .contramap(identity[ChronoLocalDateTime[_ <: ChronoLocalDate]])
  }

  @deprecated("Use orderLocalDate instead.", "1.0.0")
  def latestLocalTimeFirst: Order[LocalTime] = orderLocalTime

  implicit val orderLocalTime: Order[LocalTime] = modify {
    Order.fromComparable
  }

  @deprecated("Use orderLocalDate instead.", "1.0.0")
  def latestInstantFirst: Order[Instant] = orderInstant

  implicit val orderInstant: Order[Instant] = modify {
    Order.fromComparable
  }

  @deprecated("Use orderLocalDate instead.", "1.0.0")
  def latestZonedDateTimeFirst: Order[ZonedDateTime] = orderZonedDateTime

  implicit val orderZonedDateTime: Order[ZonedDateTime] = modify {
    Order
      .fromComparable[ChronoZonedDateTime[_ <: ChronoLocalDate]]
      .contramap(identity[ChronoZonedDateTime[_ <: ChronoLocalDate]])
  }
}

object TimeOrder {

  object YoungestFirst extends TimeOrder {
    override protected def modify[T](order: Order[T]): Order[T] = Order.reverse(order)
  }

  object OldestFirst extends TimeOrder
}
