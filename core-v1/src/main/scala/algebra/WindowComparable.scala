package com.rallyhealth.vapors.v1

package algebra

import data.{Justified, Window}
import debug.HasShow
import cats.Show
import cats.data.{NonEmptyList, NonEmptySeq}
import shapeless.Id

/**
  * Defines the capability for performing a comparison over a wrapper type.
  *
  * This is primarily used by the [[Expr.WithinWindow]] interpreter to take the result of a value expression
  * and a window expression and perform the appropriate action to produce a wrapped boolean result.
  *
  * @tparam W a wrapper type or effect to map over when performing the comparison
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
trait WindowComparable[W[_], OP[_]] {

  def withinWindow[V](
    value: W[V],
    window: W[Window[V]],
  )(implicit
    opV: OP[W[V]],
    opW: OP[W[Window[V]]],
  ): W[Boolean]
}

object WindowComparable {

  private val anyJustified: WindowComparable[Justified, Any] = {
    new WindowComparable[Justified, Any] {

      override def withinWindow[V](
        value: Justified[V],
        window: Justified[Window[V]],
      )(implicit
        opV: Any,
        opW: Any,
      ): Justified[Boolean] = {
        implicit val showV: Show[V] = opV match {
          case has: HasShow[_] => has.show.asInstanceOf[Show[V]]
          case _ => Show.fromToString
        }
        val comparison = Window.showWindowWithTerm[V]("_").show(window.value)
        val isWithinWindow = window.value.contains(value.value)
        Justified.byInference(comparison, isWithinWindow, NonEmptySeq(value, Vector(window)))
      }
    }
  }

  implicit def justified[OP[_]]: WindowComparable[Justified, OP] =
    anyJustified.asInstanceOf[WindowComparable[Justified, OP]]

  private val anyIdentity: WindowComparable[Id, Any] = new WindowComparable[Id, Any] {
    override def withinWindow[V](
      value: V,
      window: Window[V],
    )(implicit
      opV: Any,
      opW: Any,
    ): Boolean = window.contains(value)
  }

  implicit def identity[OP[_]]: WindowComparable[Id, OP] = anyIdentity.asInstanceOf[WindowComparable[Id, OP]]
}
