package com.rallyhealth.vapors.v1

package debug

import sourcecode.{File, Line}

/**
  * A collection of useful information about the source code where this implicit is summoned.
  *
  * The compiler will invoke a macro at compile-time to materialize an implicit value of this type.
  *
  * @see <a href="https://github.com/com-lihaoyi/sourcecode">sourcecode</a> for more details
  *
  * @param file the file where this implicit object was summoned from the compiler
  * @param line the line of code where this implicit object was summoned from the compiler
  */
final class SourceCodeInfo private (
  val file: File,
  val line: Line,
) extends Product2[File, Line] {
  override def _1: File = file
  override def _2: Line = line
  override def canEqual(that: Any): Boolean = that.isInstanceOf[SourceCodeInfo]
}

object SourceCodeInfo {

  def apply(
    file: File,
    line: Line,
  ): SourceCodeInfo = new SourceCodeInfo(file, line)

  def unapply(sourceCodeInfo: SourceCodeInfo): Some[(File, Line)] = Some((sourceCodeInfo.file, sourceCodeInfo.line))

  /**
    * Summons the source code info from the compiler about where this implicit is being requested.
    */
  implicit def here(
    implicit
    file: File,
    line: Line,
  ): SourceCodeInfo = SourceCodeInfo(file, line)
}
