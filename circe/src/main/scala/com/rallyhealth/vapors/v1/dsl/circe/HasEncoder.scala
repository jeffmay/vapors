package com.rallyhealth

package vapors.v1.dsl.circe

import io.circe.Encoder

trait HasEncoder[O] {

  def encodeOutput: Encoder[O]
}

object HasEncoder {

  @inline def apply[O : HasEncoder]: HasEncoder[O] = implicitly

  implicit def has[O : Encoder]: HasEncoder[O] = new HasEncoder[O] {
    override final val encodeOutput: Encoder[O] = Encoder[O]
  }
}
