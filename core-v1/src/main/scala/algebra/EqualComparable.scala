package com.rallyhealth.vapors.v1

package algebra

import cats.{Eq, Functor, Semigroupal}
import shapeless3.deriving.Id

/**
  * Defines equality over an effect type `F` with a provided param `OP`
  *
  * @tparam W the wrapper (or effect) type over which equality is computed
  * @tparam V the value type to compare for equality
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
trait EqualComparable[W[_], V, OP[_]] {

  def isEqual(
    left: W[V],
    right: W[V],
  )(implicit
    opV: OP[W[V]],
    opO: OP[W[Boolean]],
  ): W[Boolean]
}

object EqualComparable extends LowPriorityEqualComparable {

  implicit def eq[V : Eq, OP[_]]: EqualComparable[Id, V, OP] =
    new EqualComparable[Id, V, OP] {
      override final def isEqual(
        left: V,
        right: V,
      )(implicit
        opV: OP[V],
        opO: OP[Boolean],
      ): Boolean = Eq[V].eqv(left, right)
    }
}

sealed trait LowPriorityEqualComparable {

  implicit def semigroupalFunctorEq[W[_] : Functor : Semigroupal, V : Eq, OP[_]]: EqualComparable[W, V, OP] =
    new EqualComparable[W, V, OP] {
      override final def isEqual(
        left: W[V],
        right: W[V],
      )(implicit
        opV: OP[W[V]],
        opO: OP[W[Boolean]],
      ): W[Boolean] = {
        import cats.syntax.apply._
        (left, right).mapN(Eq[V].eqv)
      }
    }
}
