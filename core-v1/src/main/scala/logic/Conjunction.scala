package com.rallyhealth.vapors.v1

package logic

import shapeless3.deriving.Id

import scala.annotation.implicitNotFound

/**
  * Defines the `AND` operation for values of the same type [[B]] over some wrapper (or effect) type [[W]], while
  * being provided a custom output parameter of type [[OP]].
  *
  * TODO: Should this allow short-circuiting? Maybe return an Option or Eval?
  *
  * @tparam W the wrapper (or effect) type over which equality is computed
  * @tparam B the boolean-like value type to perform conjunction
  * @tparam OP a custom output parameter type used by visitors to enable post-processing operations.
  *            See [[dsl.DslTypes.OP]] for more details.
  */
@implicitNotFound(
  """
Cannot perform a logical AND operation on values of type ${W}[${B}] with an output parameter of ${OP}.

To define conjunction for a custom type (aka the AND operation), you must define or import an instance of Conjunction[${W}, ${B}, ${OP}]""",
)
trait Conjunction[W[_], B, OP[_]] {

  def and(
    left: W[B],
    right: W[B],
  )(implicit
    opB: OP[W[B]],
  ): W[B]
}

object Conjunction {

  @inline implicit final def bool[OP[_]]: Conjunction[Id, Boolean, OP] = Logic.bool
}
