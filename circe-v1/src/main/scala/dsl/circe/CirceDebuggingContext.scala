package com.rallyhealth.vapors.v1

package dsl.circe

import debug.{HasSourceCodeInfo, SourceCodeInfo}

import io.circe.Encoder

trait CirceDebuggingContext[O] extends HasEncoder[O] with HasSourceCodeInfo

object CirceDebuggingContext {
  implicit def here[O : Encoder](implicit info: SourceCodeInfo): CirceDebuggingContext[O] =
    new CirceDebuggingContext[O] {
      override final val encodeOutput: Encoder[O] = implicitly
      override final val debugSource: SourceCodeInfo = info
    }
}
