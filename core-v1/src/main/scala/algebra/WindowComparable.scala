package com.rallyhealth.vapors.v1

package algebra

import data.{Justified, Window}
import debug.HasShow

import cats.data.NonEmptyList
import cats.{Id, Show}

trait WindowComparable[F[_], OP[_]] {

  // TODO: Should this take OP[V] and OP[Window[V]]? What about the F[_] wrapper?
  //       For now, I am only taking the OP[V] because if the goal is to serialize the value and the window,
  //       then that is the missing piece.
  def withinWindow[V](
    value: F[V],
    window: F[Window[V]],
  )(implicit
    opV: OP[V],
  ): F[Boolean]
}

object WindowComparable {

  private val anyJustified: WindowComparable[Justified, Any] = {
    new WindowComparable[Justified, Any] {

      override def withinWindow[V](
        value: Justified[V],
        window: Justified[Window[V]],
      )(implicit
        opV: Any,
      ): Justified[Boolean] = {
        implicit val showV: Show[V] = opV match {
          case has: HasShow[_] => has.show.asInstanceOf[Show[V]]
          case _ => Show.fromToString
        }
        val comparison = Window.showWindowWithTerm[V]("_").show(window.value)
        val isWithinWindow = Window.contains(window.value, value.value)
        // TODO: Combine evidence of window with evidence of value?
        Justified.byInference(comparison, isWithinWindow, NonEmptyList.of(value, window))
      }
    }
  }

  implicit def justified[OP[_]]: WindowComparable[Justified, OP] =
    anyJustified.asInstanceOf[WindowComparable[Justified, OP]]

  private val anyIdentity: WindowComparable[Lambda[a => a], Any] = new WindowComparable[Lambda[a => a], Any] {
    override def withinWindow[V](
      value: V,
      window: Window[V],
    )(implicit
      opV: Any,
    ): Boolean = Window.contains(window, value)
  }

  implicit def identity[OP[_]]: WindowComparable[Id, OP] = anyIdentity.asInstanceOf[WindowComparable[Id, OP]]
}
