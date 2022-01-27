package com.rallyhealth.vapors.v1

package math

import scala.annotation.implicitNotFound

@implicitNotFound("""${L} * ${R} is not supported.
                     
If these are non-numeric types, try swapping the order of arguments to ${R} * ${L}.
                     
If you think this operation should be allowed, you can define an implicit Multiply[${L}, ${R}].""")
trait Multiply[L, R] {
  type Out

  def multiply(
    left: L,
    right: R,
  ): Out

  /**
    * Swap the order of arguments to the combine method. This is based on the assumption that multiplication
    * is always commutative between the left and right argument types.
    */
  final def swapArgs: Multiply.Aux[R, L, Out] = Multiply.instance { (r, l) =>
    multiply(l, r)
  }
}

object Multiply extends MultiplyNumericImplicits with MultiplyStringImplicits {
  type Aux[L, R, O] = Multiply[L, R] { type Out = O }

  def instance[L, R, O](fn: (L, R) => O): Multiply.Aux[L, R, O] = new Multiply[L, R] {
    override type Out = O
    override def multiply(
      left: L,
      right: R,
    ): Out = fn(left, right)
  }

  @inline def id[N](implicit multiply: Multiply.Aux[N, N, N]): Aux[N, N, N] = multiply
}

private[math] trait MultiplyNumericImplicits extends LowPriorityMultiplyNumericImplicits {

  implicit def numeric[I : Numeric]: Multiply.Aux[I, I, I] = Multiply.instance(Numeric[I].times)
}

private[math] trait LowPriorityMultiplyNumericImplicits {

  implicit def numericCoerceLeft[L : Numeric, R : Numeric](implicit ev: R => L): Multiply.Aux[L, R, L] =
    Multiply.instance((l, r) => Numeric[L].times(l, ev(r)))

  implicit def numericCoerceRight[L : Numeric, R : Numeric](implicit ev: L => R): Multiply.Aux[L, R, R] =
    Multiply.instance((l, r) => Numeric[R].times(ev(l), r))
}

private[math] trait MultiplyStringImplicits {

  implicit val stringTimesInt: Multiply.Aux[String, Int, String] = Multiply.instance(_ * _)
}
