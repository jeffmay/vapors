package com.rallyhealth.vapors.v1

package dsl

import com.rallyhealth.vapors.v1.data.TypedFact
import shapeless.Id

trait WrapFact[W[_], OP[_]] {

  def wrapFact[O](fact: TypedFact[O])(implicit opO: OP[O]): W[O]
}

object WrapFact {

  private final object Unwrapped extends WrapFact[Id, Any] {
    override def wrapFact[O](fact: TypedFact[O])(implicit opO: Any): O = fact.value
  }

  implicit def unwrapped[OP[_]]: WrapFact[Id, OP] = Unwrapped.asInstanceOf[WrapFact[Id, OP]]
}
