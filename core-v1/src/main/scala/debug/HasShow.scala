package com.rallyhealth.vapors.v1

package debug

import dsl.WrapParam

import cats.Show
import cats.Show.ContravariantShow

/**
  * Defines a typeclass to serialize each output type value to a string.
  *
  * @see [[Show]] for more details.
  *
  * @tparam A the expression output type parameter.
  */
trait HasShow[-A] {

  def show: ContravariantShow[A]
}

object HasShow extends LowPriorityHasShow {

  inline final def apply[A : HasShow]: HasShow[A] = implicitly

  inline final def unapply[A](has: HasShow[A]): Some[ContravariantShow[A]] = Some(has.show)

  inline final def none[A]: HasShow[A] = NoShow.asInstanceOf[HasShow[A]]

  private object NoShow extends HasShow[Any] {
    override val show: ContravariantShow[Any] = Show.fromToString
  }

  private final case class Impl[-A](show: ContravariantShow[A]) extends HasShow[A]

  given [A : ContravariantShow]: HasShow[A] = Impl(summon[ContravariantShow[A]])

  given wrapParam[W[+_]](using HasShow[W[Any]]): WrapParam[W, HasShow] with {
    override def wrapParam[O](using HasShow[O]): HasShow[W[O]] = summon[HasShow[W[Any]]]
  }
}

private[debug] sealed abstract class LowPriorityHasShow {

  /**
    * There is no [[Show]] instance available for this type, so just use `.toString`
    *
    * Similar to [[dsl.NoOP.~]], this method name is short because it will show up in a lot of places and
    * it means that you can effectively ignore this parameter.
    */
  given ~ : HasShow[Any] = HasShow.none
}
