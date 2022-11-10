package com.rallyhealth.vapors.v1
package dsl

trait WrapParam[W[+_], OP[_]] {

  def wrapParam[O](using OP[O]): OP[W[O]]
}

object WrapParam {

  given wrapID[OP[_]]: WrapParam[[x] =>> x, OP] with {
    override def wrapParam[O](using OP[O]): OP[O] = summon[OP[O]]
  }
}
