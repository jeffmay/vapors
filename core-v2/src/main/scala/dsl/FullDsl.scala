package com.rallyhealth.vapors.v2
package dsl

/**
 * Defines a dependency on all of the required DSL traits, but defines nothing itself. It is meant to
 * make it clear which DSL components need to be mixed in together to make a full DSL trait, so it should
 * be the base of all fully defined DSLs.
 *
 * There are various embedded domain-specific language objects you can import from to get a nicer syntax
 * for working with these expressions. For example, you might want to work with [[justification.Justified]] values,
 * so that you can track how a certain result was obtained. The [[dsl.simple.justified]] DSL will do this
 * for you.
 */
trait FullDsl {
  self: DslTypes &
    BuildExprDsl =>
//    ExprHListDslImplicits &
//    OutputTypeImplicits &
//    MidPriorityOutputTypeImplicits &
//    LowPriorityOutputTypeImplicits &
//    RunExprDsl =>
}
