package com.rallyhealth.vapors.v1.dsl

import cats.data.NonEmptyList

trait WrapQuantifier[W[+_], OP[_]] {

  def shortCircuit: Boolean

  def wrapFalseForAll(falseResults: NonEmptyList[W[Boolean]])(implicit opB: OP[W[Boolean]]): W[Boolean]

  def wrapTrueForAll(trueResults: List[W[Boolean]])(implicit opB: OP[W[Boolean]]): W[Boolean]

  def wrapFalseExists(falseResults: List[W[Boolean]])(implicit opB: OP[W[Boolean]]): W[Boolean]

  def wrapTrueExists(trueResults: NonEmptyList[W[Boolean]])(implicit opB: OP[W[Boolean]]): W[Boolean]
}
