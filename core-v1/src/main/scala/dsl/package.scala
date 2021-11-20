package com.rallyhealth.vapors.v1

package object dsl {

  type CW[C[+_], W[+_], +A] = C[W[A]]
}
