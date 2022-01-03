package com.rallyhealth.vapors.v1

package dsl

import cats.Foldable
import shapeless.Id

trait WrapContained[W[+_], OP[_]] {

  def wrapContained[C[_] : Foldable, V](
    original: C[W[V]],
    valid: Set[W[V]],
    found: Seq[W[V]],
  )(implicit
    opV: OP[V], // TODO: Should this be W[V]? List[W[V]]? W[Set[V]]? All 3? Last 2?
  ): W[Boolean]
}

object WrapContained {

  @inline implicit final def unwrapped[OP[_]]: WrapContained[Id, OP] = Unwrapped.asInstanceOf[WrapContained[Id, OP]]

  private final object Unwrapped extends WrapContained[Id, Any] {

    override def wrapContained[C[_] : Foldable, V](
      original: C[V],
      valid: Set[V],
      found: Seq[V],
    )(implicit
      opV: Any,
    ): Boolean = found.nonEmpty
  }
}
