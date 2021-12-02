package com.rallyhealth.vapors.v1

package dsl

trait UnwrappedDslTypes extends Any with DslTypes {

  /**
    * The identity type effectively "unwraps" the wrapper type defined by [[DslTypes.W]].
    *
    * @see [[cats.Id]]
    */
  override final type W[+A] = A
}
