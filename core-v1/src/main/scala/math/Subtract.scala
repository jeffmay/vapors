package com.rallyhealth.vapors.v1

package math

import java.time.{Duration, Instant, LocalDate, Period}
import scala.annotation.implicitNotFound

@implicitNotFound("""${L} - ${R} is not supported.
                     
If these are non-numeric types, try swapping the order of arguments to ${R} - ${L}.

If you think this operation should be allowed, you can define an implicit Subtract[${L}, ${R}].""")
trait Subtract[L, R] {
  type Out

  def subtract(
    left: L,
    right: R,
  ): Out
}

object Subtract extends SubtractNumericImplicits with SubtractJavaTimeImplicits {
  type Aux[L, R, O] = Subtract[L, R] { type Out = O }

  def apply[L, R, O](fn: (L, R) => O): Subtract.Aux[L, R, O] = new Subtract[L, R] {
    override type Out = O
    override def subtract(
      left: L,
      right: R,
    ): Out = fn(left, right)
  }

  @inline def id[N](implicit sub: Subtract.Aux[N, N, N]): Aux[N, N, N] = sub

}

private[math] trait SubtractNumericImplicits extends LowPrioritySubtractNumericImplicits {

  implicit def numeric[I : Numeric]: Subtract.Aux[I, I, I] = Subtract(Numeric[I].minus)
}

private[math] trait LowPrioritySubtractNumericImplicits {

  implicit def numericCoerceLeft[L : Numeric, R : Numeric](implicit ev: R => L): Subtract.Aux[L, R, L] =
    Subtract(Numeric[L].minus(_, _))

  implicit def numericCoerceRight[L : Numeric, R : Numeric](implicit ev: L => R): Subtract.Aux[L, R, R] =
    Subtract(Numeric[R].minus(_, _))
}

private[math] trait SubtractJavaTimeImplicits {

  implicit val subtractDurationFromInstant: Subtract.Aux[Instant, Duration, Instant] = Subtract(_.minus(_))

  implicit val subtractPeriodFromLocalDate: Subtract.Aux[LocalDate, Period, LocalDate] = Subtract(_.minus(_))
}
