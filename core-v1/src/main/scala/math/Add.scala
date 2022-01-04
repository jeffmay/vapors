package com.rallyhealth.vapors.v1

package math

import java.time.{Duration, Instant, LocalDate, Period}
import scala.annotation.implicitNotFound

@implicitNotFound("""${L} + ${R} is not supported.
                     
If these are non-numeric types, try swapping the order of arguments to ${R} + ${L}.
                     
If you think this operation should be allowed, you can define an implicit Add[${L}, ${R}].""")
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

  def instance[L, R, O](fn: (L, R) => O): Add.Aux[L, R, O] = new Add[L, R] {
    override type Out = O
    override def add(
      left: L,
      right: R,
    ): Out = fn(left, right)
  }

  @inline def id[N](implicit add: Add.Aux[N, N, N]): Aux[N, N, N] = add
}

private[math] trait AddNumericImplicits extends LowPriorityAddNumericImplicits {

  implicit def numeric[I : Numeric]: Add.Aux[I, I, I] = Add.instance(Numeric[I].plus)
}

private[math] trait LowPriorityAddNumericImplicits {

  implicit def numericCoerceLeft[L : Numeric, R : Numeric](implicit ev: R => L): Add.Aux[L, R, L] =
    Add.instance(Numeric[L].plus(_, _))

  implicit def numericCoerceRight[L : Numeric, R : Numeric](implicit ev: L => R): Add.Aux[L, R, R] =
    Add.instance(Numeric[R].plus(_, _))
}

private[math] trait AddJavaTimeImplicits {

  implicit val addDurationToInstant: Add.Aux[Instant, Duration, Instant] = Add.instance(_.plus(_))

  implicit val addPeriodToLocalDate: Add.Aux[LocalDate, Period, LocalDate] = Add.instance(_.plus(_))
}
