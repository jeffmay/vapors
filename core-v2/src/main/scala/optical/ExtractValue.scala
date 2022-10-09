package com.rallyhealth.vapors.v2
package optical

trait ExtractValue[-A, +B] {
  def extract(input: A): B
}

object ExtractValue {
  given extractId[A]: ExtractValue[A, A] = identity(_)
}
