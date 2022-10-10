package com.rallyhealth.vapors.v2
package justification

import zio.prelude.NonEmptySet

/**
 * Represents a result that contains a tree of justified inputs and operations, as well as the value
 * for each operation along the way.
 *
 * Values can be justified by:
 * - Constants embedded into the expression itself
 * - Config values that are embedded into the expression but have a known configuration key / description
 * - Facts from the [[data.FactTable]] used to compute this output
 * - Inference by an operation defined by the [[dsl.JustifiedBuildExprDsl]] with references to all the
 *   justified values used as inputs to the operation.
 *
 * By following the chain of justification, one can determine if there is any factual evidence for a value,
 * or whether it is only supported by configs and constants (i.e. it is a default value and not one that is
 * tailored based on the provided facts).
 *
 * @tparam V the type of value that is justified by this container
 */
final case class Justified[+V](value: V, justification: Justification[V])

enum Justification[+V](reason: String) {

  case ByConst extends Justification("const")

  case ByInference(reason: String, premises: NonEmptySet[Justified[Any]]) extends Justification(reason)

}

