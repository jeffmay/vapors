package com.rallyhealth.vapors.v1

package debug

import cats.Show

/**
  * Defines a typeclass to serialize each output type value to a string.
  *
  * @see [[Show]] for more details.
  *
  * @tparam A the expression output type parameter.
  */
trait HasShow[A] {

  def show: Show[A]
}

object HasShow extends LowPriorityHasShow {

  inline final def apply[A : HasShow]: HasShow[A] = implicitly

  inline final def unapply[A](has: HasShow[A]): Some[Show[A]] = Some(has.show)

  inline final def none[A]: HasShow[A] = NoShow.asInstanceOf[HasShow[A]]

  private object NoShow extends HasShow[Any] {
    override val show: Show[Any] = _.toString
  }

  private final case class Impl[A](show: Show[A]) extends HasShow[A]

  given [A : Show]: HasShow[A] = Impl(Show[A])
}

private[debug] sealed abstract class LowPriorityHasShow {

  /**
    * There is no [[Show]] instance available for this type, so just use `.toString`
    *
    * Similar to [[dsl.NoOP.~]], this method name is short because it will show up in a lot of places and
    * it means that you can effectively ignore this parameter.
    */
  given ~[A]: HasShow[A] = HasShow.none
}
