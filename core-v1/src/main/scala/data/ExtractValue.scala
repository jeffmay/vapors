package com.rallyhealth.vapors.v1

package data

/**
  * Extracts a value from some starting value.
  *
  * While definitionally identical to an implicit function, it's meaning is different. This
  * is typically used to extract a value from a field of a product type.
  */
trait ExtractValue[-T, +V] extends (T => V) {

  override final def apply(v1: T): V = extractValue(v1)

  def extractValue(obj: T): V
}

object ExtractValue {

  type As[-T] = [v] =>> ExtractValue[T, v]

  /**
    * A handy type alias for extracting a boolean from a given type.
    *
    * To create your own alias, you'll need Scala 3 or the type-projector plugin.
    *
    * This alias makes it easy to add as a context bound (i.e. the `:` operator)
    */
  type AsBoolean[-T] = ExtractValue[T, Boolean]

  def asBoolean[T](value: T)(implicit extractor: AsBoolean[T]): Boolean = extractor.extractValue(value)

  final def of[V]: Of[V] = new Of[V]
  final class Of[V] private[ExtractValue] (private val dummy: Boolean = true) extends AnyVal {

    def apply[T](input: T)(implicit extractor: ExtractValue[T, V]): V = extractor.extractValue(input)

    def from[T](implicit extractor: ExtractValue[T, V]): ExtractValue[T, V] = extractor
  }

  implicit def conforms[A, B](implicit ev: A => B): ExtractValue[A, B] = ev.apply(_)

  implicit def extracted[W[_] : Extract, V]: ExtractValue[W[V], V] = Extract[W].extract(_)
}
