package com.rallyhealth.vapors.v1

package object dsl {

  /**
    * A helpful type-level function for double-wrapped element of type [[A]].
    *
    * First wrapped by some covariant collection type [[C]], where every element is wrapped again
    * by a wrapper type [[W]].
    */
  type CW[C[+_], W[+_], +A] = C[W[A]]
}
