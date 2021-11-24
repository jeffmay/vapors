package com.rallyhealth.vapors.v1

package debug

import cats.Show

trait HasDebugSourceInfo[A] extends HasShow[A] with HasSourceCodeInfo

object HasDebugSourceInfo {

  @inline final def apply[A : HasDebugSourceInfo]: HasDebugSourceInfo[A] = implicitly

  private final case class Impl[A](
    show: Show[A],
    debugSource: SourceCodeInfo,
  ) extends HasDebugSourceInfo[A]

  implicit def here[A](
    implicit
    show: HasShow[A],
    debugSource: SourceCodeInfo,
  ): HasDebugSourceInfo[A] = Impl(show.show, debugSource)
}
