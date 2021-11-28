package com.rallyhealth.vapors.v1

package logic

import shapeless.Id

trait Logic[F[_], B, OP[_]] extends Conjunction[F, B, OP] with Disjunction[F, B, OP] with Negation[F, B, OP]

object Logic {

  implicit def bool[OP[_]]: Logic[Id, Boolean, OP] = AnyBool.asInstanceOf[Logic[Id, Boolean, OP]]

  final object AnyBool extends Logic[Id, Boolean, Any] {

    override def and(
      left: Boolean,
      right: Boolean,
    )(implicit
      opB: Any,
    ): Boolean = left && right

    override def or(
      left: Boolean,
      right: Boolean,
    )(implicit
      opB: Any,
    ): Boolean = left || right

    override def not(value: Boolean)(implicit opB: Any): Boolean = !value
  }

}
