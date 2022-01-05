package com.rallyhealth.vapors.v1

package data

import cats.syntax.contravariant._
import cats.Order

import java.time._
import java.time.chrono.{ChronoLocalDate, ChronoLocalDateTime, ChronoZonedDateTime}

abstract class TimeOrder {

  def modify[T](order: Order[T]): Order[T]

  implicit val latestLocalDateFirst: Order[LocalDate] = modify {
    Order.fromComparable[ChronoLocalDate].contramap(identity[ChronoLocalDate])
  }

  implicit val latestLocalDateTimeFirst: Order[LocalDateTime] = modify {
    Order
      .fromComparable[ChronoLocalDateTime[_ <: ChronoLocalDate]]
      .contramap(identity[ChronoLocalDateTime[_ <: ChronoLocalDate]])
  }

  implicit val latestLocalTimeFirst: Order[LocalTime] = modify {
    Order.fromComparable
  }

  implicit val latestInstantFirst: Order[Instant] = modify {
    Order.fromComparable
  }

  implicit val latestZonedDateTimeFirst: Order[ZonedDateTime] = modify {
    Order
      .fromComparable[ChronoZonedDateTime[_ <: ChronoLocalDate]]
      .contramap(identity[ChronoZonedDateTime[_ <: ChronoLocalDate]])
  }
}

object TimeOrder {

  final object YoungestFirst extends TimeOrder {
    override def modify[T](order: Order[T]): Order[T] = Order.reverse(order)
  }

  final object OldestFirst extends TimeOrder {
    override def modify[T](order: Order[T]): Order[T] = Order.reverse(order)
  }
}
