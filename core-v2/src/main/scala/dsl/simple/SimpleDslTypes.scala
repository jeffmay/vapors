package com.rallyhealth.vapors.v2
package dsl.simple

import dsl.DslTypes

/**
 * A "simple" DSL returns unwrapped results.
 */
trait SimpleDslTypes extends Any with DslTypes {

  /**
   * The identity type effectively "unwraps" the wrapper type defined by [[DslTypes.W]].
   */
  override final type W[+A] = A
}
