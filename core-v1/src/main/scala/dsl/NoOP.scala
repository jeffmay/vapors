package com.rallyhealth.vapors.v1

package dsl

/**
  * A type for which there is always an implicit value.
  *
  * @note this is the same as [[DummyImplicit]] but with a shorter name for IDE readability.
  */
final class NoOP private {}

object NoOP {

  /** The only instance of this dummy implicit value */
  implicit final val ~ : NoOP = new NoOP
}
