package com.rallyhealth.vapors.v1

package math

import scala.annotation.implicitNotFound

@implicitNotFound("""${L} ^ ${R} is not supported (aka pow(${L}, ${R})).

If you think this operation should be allowed, you can define an implicit Power[${L}, ${R}].""")
trait Power[L, R] {
  type Out

  def pow(
    left: L,
    right: R,
  ): Out
}

object Power extends PowerNumericImplicits {
  type Aux[L, R, O] = Power[L, R] { type Out = O }

  def apply[L, R, O](fn: (L, R) => O): Power.Aux[L, R, O] = new Power[L, R] {
    override type Out = O
    override def pow(
      left: L,
      right: R,
    ): Out = fn(left, right)
  }

  @inline def id[N](implicit power: Power.Aux[N, N, N]): Aux[N, N, N] = power
}

private[math] trait PowerNumericImplicits {

  implicit def numeric[L : Numeric, R : Numeric]: Power.Aux[L, R, Double] = {
    val L = Numeric[L]
    val R = Numeric[R]
    Power { (l, r) =>
      Math.pow(L.toDouble(l), R.toDouble(r))
    }
  }
}
