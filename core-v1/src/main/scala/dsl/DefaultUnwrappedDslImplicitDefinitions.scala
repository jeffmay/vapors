package com.rallyhealth.vapors.v1.dsl

trait DefaultUnwrappedDslImplicitDefinitions extends UnwrappedDslTypes {

  protected final val defn: DslImplicitDefinitions[W, OP] = new DslImplicitDefinitions[W, OP]
}
