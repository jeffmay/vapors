package com.rallyhealth

package vapors.v1.math

import java.time.{Duration, Instant, LocalDate, Period}
import scala.annotation.implicitNotFound

@implicitNotFound(
  "Cannot add ${R} to ${L} with the + operator. If this is incorrect, please define an implicit Add[${L}, ${R}].",
)
trait Add[-L, -R] {
  type Out

  def combine(
    left: L,
    right: R,
  ): Out

  final def swapArgs: Add.Aux[R, L, Out] = Add { (r, l) =>
    combine(l, r)
  }
}

object Add extends NumericAddImplicits with JavaTimeAddImplicits {
  type Aux[-L, -R, O] = Add[L, R] { type Out = O }

  def apply[L, R, O](fn: (L, R) => O): Add.Aux[L, R, O] = new Add[L, R] {
    override type Out = O
    override def combine(
      left: L,
      right: R,
    ): Out = fn(left, right)
  }

  @inline def id[N](implicit add: Add.Aux[N, N, N]): Aux[N, N, N] = add

}

trait NumericAddImplicits extends LowPriorityNumericAddImplicits {

  implicit def numeric[I : Numeric]: Add.Aux[I, I, I] = Add(Numeric[I].plus)
}

trait LowPriorityNumericAddImplicits {

  implicit def numericCoerceLeft[L : Numeric, R : Numeric](implicit ev: R => L): Add.Aux[L, R, L] =
    Add(Numeric[L].plus(_, _))

  implicit def numericCoerceRight[L : Numeric, R : Numeric](implicit ev: L => R): Add.Aux[L, R, R] =
    Add(Numeric[R].plus(_, _))
}

trait JavaTimeAddImplicits {

  implicit val addDurationToInstant: Add.Aux[Instant, Duration, Instant] = Add(_.plus(_))

  implicit val addInstantToDuration: Add.Aux[Duration, Instant, Instant] = addDurationToInstant.swapArgs

  implicit val addPeriodToLocalDate: Add.Aux[LocalDate, Period, LocalDate] = Add(_.plus(_))

  implicit val addLocalDateToPeriod: Add.Aux[Period, LocalDate, LocalDate] = addPeriodToLocalDate.swapArgs
}
