package com.rallyhealth.vapors.v1

package data

sealed abstract class Justified[+V] {
  def value: V
  def withValue[U](value: U): Justified[U]

  def evidenceOrNone: Evidence = Evidence.none

  final def map[U](fn: V => U): Justified[U] = withValue(fn(value))
}

object Justified {

  final case class Constant[+V](value: V) extends Justified[V] {
    override def withValue[U](value: U): Justified[U] = copy(value = value)
  }

  final case class ByConfig[+V](
    value: V,
    configKey: String,
    configDescription: Option[String] = None,
  ) extends Justified[V] {
    override def withValue[U](value: U): Justified[U] = copy(value = value)
  }

  final case class ByEvidence[+V](
    value: V,
    evidence: Evidence,
  ) extends Justified[V] {
    override def withValue[U](value: U): Justified[U] = copy(value = value)
    override def evidenceOrNone: Evidence = evidence
  }
}
