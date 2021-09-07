package com.rallyhealth

package vapors.v1.dsl.circe

import io.circe.Encoder

trait HasEncoder[O] {

  def encodeOutput: Encoder[O]
}
