package com.rallyhealth.vapors.v1

package math

import java.time.{Duration, Instant, LocalDate, LocalDateTime, Period, ZonedDateTime}
import scala.annotation.implicitNotFound
import scala.concurrent.duration.FiniteDuration

@implicitNotFound("""(${L} + ${R}) is not supported.

If these are non-numeric types, try swapping the arguments.
To explicitly allow this operation, define a given Add[L, R] for these types.""")
trait Add[L, R] {
  type Out

  def add(
    left: L,
    right: R,
  ): Out

  /**
    * Swap the order of arguments to the combine method. This is based on the assumption that addition
    * is always commutative between the left and right argument types.
    */
  final def swapArgs: Add.Aux[R, L, Out] = Add.instance { (r, l) =>
    add(l, r)
  }
}

object Add extends AddNumericImplicits with AddJavaTimeImplicits {
  type Aux[L, R, O] = Add[L, R] { type Out = O }
  type Id[N] = Add[N, N] { type Out = N }

  def instance[L, R, O](fn: (L, R) => O): Add.Aux[L, R, O] = new Add[L, R] {
    override type Out = O
    override def add(
      left: L,
      right: R,
    ): Out = fn(left, right)
  }

  inline def id[N](implicit add: Add.Aux[N, N, N]): Aux[N, N, N] = add
}

private[math] trait AddNumericImplicits extends LowPriorityAddNumericImplicits {

  implicit def numeric[I : Numeric]: Add.Aux[I, I, I] = Add.instance(Numeric[I].plus)
}

private[math] trait LowPriorityAddNumericImplicits {

  implicit def numericCoerceLeft[L : Numeric, R : Numeric](implicit ev: R <:< L): Add.Aux[L, R, L] =
    Add.instance((l, r) => Numeric[L].plus(l, r))

  implicit def numericCoerceRight[L : Numeric, R : Numeric](implicit ev: L <:< R): Add.Aux[L, R, R] =
    Add.instance((l, r) => Numeric[R].plus(l, r))
}

private[math] trait AddJavaTimeImplicits {

  implicit val addDurationToInstant: Add.Aux[Instant, Duration, Instant] = Add.instance(_.plus(_))

  implicit val addPeriodToLocalDate: Add.Aux[LocalDate, Period, LocalDate] = Add.instance(_.plus(_))

  implicit val addFiniteDurationToInstant: Add.Aux[Instant, FiniteDuration, Instant] = Add.instance {
    (temporal, duration) =>
      temporal.plusNanos(duration.toNanos)
  }

  implicit val addFiniteDurationToLocalDate: Add.Aux[LocalDate, FiniteDuration, LocalDate] = Add.instance {
    (temporal, duration) =>
      temporal.plusDays(duration.toDays)
  }

  implicit val addFiniteDurationToLocalDateTime: Add.Aux[LocalDateTime, FiniteDuration, LocalDateTime] = Add.instance {
    (temporal, duration) =>
      temporal.plusNanos(duration.toNanos)
  }

  implicit val addFiniteDurationToZonedDateTime: Add.Aux[ZonedDateTime, FiniteDuration, ZonedDateTime] = Add.instance {
    (temporal, duration) =>
      temporal.plusNanos(duration.toNanos)
  }
}
