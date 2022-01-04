package com.rallyhealth.vapors.v1

package math

import scala.annotation.implicitNotFound

@implicitNotFound("""${B} ^ ${E} is not supported (aka pow(${B}, ${E})).
                     
If these are non-numeric types, you can try swapping the order of arguments to ${E} ^ ${B}.

If you think this operation should be allowed, you can define an implicit Power[${B}, ${E}].""")
trait Power[B, E] {
  type Out

  def power(
    base: B,
    exponent: E,
  ): Out
}

object Power extends PowerNumericImplicits {
  type Aux[B, E, O] = Power[B, E] { type Out = O }

  def instance[B, E, O](fn: (B, E) => O): Power.Aux[B, E, O] = new Power[B, E] {
    override type Out = O
    override def power(
      base: B,
      exponent: E,
    ): Out = fn(base, exponent)
  }

  @inline def id[N](implicit power: Power.Aux[N, N, N]): Aux[N, N, N] = power
}

private[math] trait PowerNumericImplicits {

  implicit def numeric[B : Numeric, E : Numeric]: Power.Aux[B, E, Double] = {
    val L = Numeric[B]
    val R = Numeric[E]
    Power.instance { (l, r) =>
      Math.pow(L.toDouble(l), R.toDouble(r))
    }
  }
}
