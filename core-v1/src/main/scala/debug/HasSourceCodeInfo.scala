package com.rallyhealth.vapors.v1

package debug

import sourcecode.{File, Line}

trait HasSourceCodeInfo {

  def debugSource: SourceCodeInfo
}

object HasSourceCodeInfo {

  def fromContext[P[_] <: HasSourceCodeInfo, A](implicit op: P[A]): HasSourceCodeInfo = op
}

final case class SourceCodeInfo(
  file: File,
  line: Line,
)

object SourceCodeInfo {

  implicit def here(
    implicit
    file: File,
    line: Line,
  ): SourceCodeInfo = SourceCodeInfo(file, line)
}
