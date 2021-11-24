package com.rallyhealth.vapors.v1

package debug

import sourcecode.{File, Line}

/**
  * Extend this trait in your custom output parameter type to make the source code information
  * where each node is constructed available to the interpreter and in post-processing.
  *
  * @see [[SourceCodeInfo]] for more details about how this works.
  */
trait HasSourceCodeInfo {

  def debugSource: SourceCodeInfo
}

object HasSourceCodeInfo {

  private final case class Impl(debugSource: SourceCodeInfo) extends HasSourceCodeInfo

  implicit def here(
    implicit
    file: File,
    line: Line,
  ): HasSourceCodeInfo = Impl(SourceCodeInfo(file, line))

  def fromContext[P[_] <: HasSourceCodeInfo, A](implicit op: P[A]): HasSourceCodeInfo = {

    /**
      * Intellij IDEA incorrectly highlights this as an error.
      * @see https://youtrack.jetbrains.com/issue/SCL-19326
      */
    op
  }
}
