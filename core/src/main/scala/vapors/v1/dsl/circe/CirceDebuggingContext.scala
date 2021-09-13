package com.rallyhealth

package vapors.v1.dsl.circe

import vapors.v1.debug.{HasSourceCodeInfo, SourceCodeInfo}

import io.circe.Encoder

trait CirceDebuggingContext[O] extends HasEncoder[O] with HasSourceCodeInfo

object CirceDebuggingContext {
  implicit def here[O : Encoder](implicit info: SourceCodeInfo): CirceDebuggingContext[O] =
    new CirceDebuggingContext[O] {
      override final val encodeOutput: Encoder[O] = implicitly
      override final val debugSource: SourceCodeInfo = info
    }
}
