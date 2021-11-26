package com.rallyhealth.vapors.v1

package math

import scala.annotation.implicitNotFound

@implicitNotFound("""${L} / ${R} is not supported.
                     
If these are non-numeric types, try swapping the order of arguments to ${R} / ${L}.

If you think this operation should be allowed, you can define an implicit Divide[${L}, ${R}].""")
trait Divide[L, R] {
  type Out

  def divide(
    left: L,
    right: R,
  ): Out
}

object Divide extends DivideNumericImplicits {
  type Aux[L, R, O] = Divide[L, R] { type Out = O }

  def apply[L, R, O](fn: (L, R) => O): Divide.Aux[L, R, O] = new Divide[L, R] {
    override type Out = O
    override def divide(
      left: L,
      right: R,
    ): Out = fn(left, right)
  }

  @inline def id[N](implicit sub: Divide.Aux[N, N, N]): Aux[N, N, N] = sub

}

private[math] trait DivideNumericImplicits extends MidPriorityDivideFractionalImplicits {

  implicit def fractional[I : Fractional]: Divide.Aux[I, I, I] = Divide(Fractional[I].div)

  implicit def integral[I : Integral]: Divide.Aux[I, I, I] = Divide(Integral[I].quot)
}

private[math] trait MidPriorityDivideFractionalImplicits extends LowPriorityDivideNumericImplicits {

  implicit def fractionalCoerceLeft[L : Fractional, R](implicit ev: R => L): Divide.Aux[L, R, L] =
    Divide(Fractional[L].div(_, _))

  implicit def fractionalCoerceRight[L, R : Fractional](implicit ev: L => R): Divide.Aux[L, R, R] =
    Divide(Fractional[R].div(_, _))
}

private[math] trait LowPriorityDivideNumericImplicits {

  implicit def integralCoerceLeft[L : Integral, R](implicit ev: R => L): Divide.Aux[L, R, L] =
    Divide(Integral[L].quot(_, _))

  implicit def integralCoerceRight[L, R : Integral](implicit ev: L => R): Divide.Aux[L, R, R] =
    Divide(Integral[R].quot(_, _))
}
