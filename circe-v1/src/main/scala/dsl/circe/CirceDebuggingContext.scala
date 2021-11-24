package com.rallyhealth.vapors.v1

package dsl.circe

import debug.{HasDebugSourceInfo, SourceCodeInfo}

import cats.Show
import io.circe.Encoder

trait CirceDebuggingContext[O] extends HasEncoder[O] with HasDebugSourceInfo[O]

object CirceDebuggingContext {

  implicit def enc[O](
    implicit
    encoder: Encoder[O],
    info: HasDebugSourceInfo[O],
  ): CirceDebuggingContext[O] =
    new CirceDebuggingContext[O] {
      override final val encodeOutput: Encoder[O] = encoder
      override final val debugSource: SourceCodeInfo = info.debugSource
      override final val show: Show[O] = info.show
    }
}
