package com.rallyhealth

package vapors.v1.math

import java.time.{Duration, Instant, LocalDate, Period}

trait Add[-L, -R, +O] {

  def combine(
    left: L,
    right: R,
  ): O

  final def swapArgs: Add[R, L, O] = { (r, l) =>
    combine(l, r)
  }
}

object Add extends NumericAddImplicits with JavaTimeAddImplicits {
  @inline def apply[L, R, O](implicit add: Add[L, R, O]): Add[L, R, O] = add

  @inline def id[N](implicit add: Add[N, N, N]): Add[N, N, N] = add
}

trait NumericAddImplicits extends LowPriorityNumericAddImplicits {

  implicit def numeric[I : Numeric]: Add[I, I, I] = Numeric[I].plus(_, _)
}

trait LowPriorityNumericAddImplicits {

  implicit def numericCoerceLeft[L : Numeric, R : Numeric](implicit ev: R => L): Add[L, R, L] = Numeric[L].plus(_, _)

  implicit def numericCoerceRight[L : Numeric, R : Numeric](implicit ev: L => R): Add[L, R, R] = Numeric[R].plus(_, _)
}

trait JavaTimeAddImplicits {

  implicit val addDurationToInstant: Add[Instant, Duration, Instant] = _.plus(_)

  implicit val addInstantToDuration: Add[Duration, Instant, Instant] = addDurationToInstant.swapArgs

  implicit val addPeriodToLocalDate: Add[LocalDate, Period, LocalDate] = _.plus(_)

  implicit val addLocalDateToPeriod: Add[Period, LocalDate, LocalDate] = addPeriodToLocalDate.swapArgs
}
