package com.rallyhealth.vapors.v1

package data

/**
  * Extracts a value from some starting value.
  */
trait ExtractValue[-T, +V] {
  def extractValue(obj: T): V
}

object ExtractValue {
  type AsBoolean[-T] = ExtractValue[T, Boolean]

  def asBoolean[T](value: T)(implicit extractor: AsBoolean[T]): Boolean = extractor.extractValue(value)

  @inline final def apply[V]: Of[V] = new Of[V]
  final class Of[V] private[ExtractValue] (private val dummy: Boolean = true) extends AnyVal {

    def apply[T](input: T)(implicit extractor: ExtractValue[T, V]): V = extractor.extractValue(input)

    def from[T](implicit extractor: ExtractValue[T, V]): ExtractValue[T, V] = extractor
  }

  implicit def conforms[A, B](implicit ev: A <:< B): ExtractValue[A, B] = ev.apply
}
