package com.rallyhealth.vapors.v1

package logic

import shapeless.Id

// TODO: Should I allow short-circuiting?
trait Conjunction[F[_], B, -OP[_]] {

  def and(
    left: F[B],
    right: F[B],
  )(implicit
    opB: OP[F[B]],
  ): F[B]
}

object Conjunction {

  @inline implicit final def bool: Conjunction[Id, Boolean, Any] = Logic.bool
}
