package com.rallyhealth.vapors.v1

package logic

import shapeless.Id

import scala.annotation.implicitNotFound

/**
  * Defines the `OR` operation for values of the same type [[B]] over some wrapper (or effect) type [[W]], while
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
Cannot perform a logical OR operation on values of type ${W}[${B}] with an output parameter of ${OP}.

To define disjunction for a custom type (aka the OR operation), you must define or import an instance of Disjunction[${W}, ${B}, ${OP}]""",
)
trait Disjunction[W[_], B, OP[_]] {

  def or(
    left: W[B],
    right: W[B],
  )(implicit
    opB: OP[W[B]],
  ): W[B]
}

object Disjunction {

  @inline implicit final def bool[OP[_]]: Disjunction[Id, Boolean, OP] = Logic.bool
}
