package com.rallyhealth.vapors.v1

package dsl

import cats.{Functor, Semigroupal}

trait DefaultUnwrappedDslImplicitDefinitions extends UnwrappedDslTypes {

  protected final val defn: DslImplicitDefinitions[W, OP] = {
    implicit val id: Functor[W] with Semigroupal[W] = cats.catsInstancesForId
    new DslImplicitDefinitions[W, OP]
  }
}
