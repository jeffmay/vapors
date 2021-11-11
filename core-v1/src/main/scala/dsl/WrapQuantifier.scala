package com.rallyhealth.vapors.v1.dsl

import cats.data.NonEmptySeq

trait WrapQuantifier[W[+_], OP[_]] {

  def shortCircuit: Boolean

  def wrapFalseForAll(falseResults: NonEmptySeq[W[Boolean]])(implicit opB: OP[W[Boolean]]): W[Boolean]

  def wrapTrueForAll(trueResults: Seq[W[Boolean]])(implicit opB: OP[W[Boolean]]): W[Boolean]

  def wrapFalseExists(falseResults: Seq[W[Boolean]])(implicit opB: OP[W[Boolean]]): W[Boolean]

  def wrapTrueExists(trueResults: NonEmptySeq[W[Boolean]])(implicit opB: OP[W[Boolean]]): W[Boolean]
}
