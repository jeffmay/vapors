package com.rallyhealth.vapors.core.data

import com.rallyhealth.vapors.core.algebra.CaptureP

/**
  * Extracts a value from some starting value.
  *
  * This is useful for defining [[CaptureP]] based on some input
  * type, without having to share a common supertype. Although, because of variance rules, this makes it
  * easy to define an [[ExtractValue]] for a supertype, if desired.
  */
trait ExtractValue[-T, +V] {
  def extractValue(obj: T): V
}

object ExtractValue {
  @inline final def apply[T, V](implicit instance: ExtractValue[T, V]): ExtractValue[T, V] = instance

  implicit def id[T]: ExtractValue[T, T] = identity[T]
}
