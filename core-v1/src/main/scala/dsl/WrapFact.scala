package com.rallyhealth.vapors.v1

package dsl

import data.TypedFact
import shapeless3.deriving.{Const, Id}

trait WrapFact[W[_], OP[_]] {

  def wrapFact[O](fact: TypedFact[O])(implicit opO: OP[O]): W[O]
}

object WrapFact {

  private object Unwrapped extends WrapFact[Id, Const[Any]] {
    override def wrapFact[O](fact: TypedFact[O])(implicit opO: Any): O = fact.value
  }

  implicit def unwrapped[OP[_]]: WrapFact[Id, OP] = Unwrapped.asInstanceOf[WrapFact[Id, OP]]
}
