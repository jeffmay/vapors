package com.rallyhealth.vapors.v1

package math

import scala.annotation.implicitNotFound

@implicitNotFound("""${N} / ${D} is not supported.
                     
If these are non-numeric types, you can try swapping the order of arguments to ${D} / ${N}.

If you think this operation should be allowed, you can define an implicit Divide[${N}, ${D}].""")
trait Divide[N, D] {
  type Out

  def divide(
    numerator: N,
    denominator: D,
  ): Out
}

object Divide extends DivideNumericImplicits {
  type Aux[N, D, O] = Divide[N, D] { type Out = O }

  def instance[N, D, O](fn: (N, D) => O): Divide.Aux[N, D, O] = new Divide[N, D] {
    override type Out = O
    override def divide(
      numerator: N,
      denominator: D,
    ): Out = fn(numerator, denominator)
  }

  @inline def id[N](implicit sub: Divide.Aux[N, N, N]): Aux[N, N, N] = sub

}

private[math] trait DivideNumericImplicits extends MidPriorityDivideFractionalImplicits {

  implicit def fractional[I : Fractional]: Divide.Aux[I, I, I] = Divide.instance(Fractional[I].div)

  implicit def integral[I : Integral]: Divide.Aux[I, I, I] = Divide.instance(Integral[I].quot)
}

private[math] trait MidPriorityDivideFractionalImplicits extends LowPriorityDivideNumericImplicits {

  implicit def fractionalCoerceLeft[N : Fractional, D](implicit ev: D => N): Divide.Aux[N, D, N] =
    Divide.instance((l, r) => Fractional[N].div(l, ev(r)))

  implicit def fractionalCoerceRight[N, D : Fractional](implicit ev: N => D): Divide.Aux[N, D, D] =
    Divide.instance((l, r) => Fractional[D].div(ev(l), r))
}

private[math] trait LowPriorityDivideNumericImplicits {

  implicit def integralCoerceLeft[N : Integral, D](implicit ev: D => N): Divide.Aux[N, D, N] =
    Divide.instance((l, r) => Integral[N].quot(l, ev(r)))

  implicit def integralCoerceRight[N, D : Integral](implicit ev: N => D): Divide.Aux[N, D, D] =
    Divide.instance((l, r) => Integral[D].quot(ev(l), r))
}
