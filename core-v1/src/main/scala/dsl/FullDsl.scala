package com.rallyhealth.vapors.v1

package dsl

/**
  * Defines a dependency on all of the required DSL traits, but defines nothing itself. It is meant to
  * make it clear which DSL components need to be mixed in together to make a full DSL trait, so it should
  * be the base of all fully defined DSLs.
  *
  * There are various embedded domain-specific language objects you can import from to get a nicer syntax
  * for working with these expressions. For example, you might want to work with [[data.Justified]] values,
  * so that you can track how a certain result was obtained. The [[dsl.simple.justified]] DSL will do this
  * for you. If you want to track the state of every operation, you can use the [[dsl.standard]] DSL and
  * apply your own visitor to the [[algebra.ExprResult]] (which is a mirror of these node types, with the
  * exception that they contain the runtime result at each step in the computation). You can even mix and
  * match various DSL capabilities by importing from [[dsl.standard.justified]].
  */
trait FullDsl {
  self: DslTypes with BuildExprDsl with RunExprDsl =>
}
