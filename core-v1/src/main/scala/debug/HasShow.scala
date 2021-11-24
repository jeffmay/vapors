package com.rallyhealth.vapors.v1

package debug

import cats.Show

/**
  * Defines a typeclass to serialize each output type value to a string.
  *
  * @see [[Show]] for more details.
  *
  * @tparam V the expression output type parameter.
  */
trait HasShow[V] {

  def show: Show[V]
}

object HasShow extends LowPriorityHasShow {

  @inline final def apply[V : HasShow]: HasShow[V] = implicitly

  final def unapply[V](has: HasShow[V]): Some[Show[V]] = Some(has.show)

  @inline final def none[V]: HasShow[V] = NoShow.asInstanceOf[HasShow[V]]

  private final object NoShow extends HasShow[Any] {
    override val show: Show[Any] = _.toString
  }

  private final case class Impl[V](show: Show[V]) extends HasShow[V]

  implicit def hasShow[V](implicit show: Show[V]): HasShow[V] = Impl(show)
}

private[debug] sealed abstract class LowPriorityHasShow {

  implicit def noShow[V]: HasShow[V] = HasShow.none
}
