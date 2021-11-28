package com.rallyhealth.vapors.v1

package logic

import shapeless.Id

// TODO: Should I allow short-circuiting?
trait Disjunction[F[_], B, -OP[_]] {

  def or(
    left: F[B],
    right: F[B],
  )(implicit
    opB: OP[F[B]],
  ): F[B]
}

object Disjunction {

  @inline implicit final def bool: Disjunction[Id, Boolean, Any] = Logic.bool
}
