package com.rallyhealth.vapors.v1

package data

import cats.data.Ior

/**
  * Defines a slice of indexes into a collection that supports negative indexing.
  *
  * For example, `1 <-> -1` would be a [[SliceRange.Relative]] slice that would return a collection with all
  * the elements of the original collection without the first or last element. If the collection has 2 elements
  * or less, then the resulting collection will be empty.
  *
  * In order to reference the relative end of a collection, you can use the [[SliceRange.End]] (ex. `3 <-> End`)
  *
  * In order to have distinct indexes, you have to call [[SliceRange.Relative.toAbsolute]] and give the size
  * of the collection that you want to slice.
  */
object SliceRange {

  /**
    * A relative slice with either a relative lower bound, upper bound, or both.
    */
  final case class Relative private (range: Ior[Int, Int]) {

    def start: Option[Int] = range.left

    def end: Option[Int] = range.right

    def toAbsolute(size: Int): Absolute = {
      val relative = range.bimap(
        relativeStart => if (relativeStart < 0) size + relativeStart else relativeStart,
        relativeEnd => if (relativeEnd < 0) size + relativeEnd else relativeEnd,
      )
      Absolute(relative.left.getOrElse(0), relative.right.getOrElse(size) - 1, this)
    }
  }

  object Relative {

    def fromEnd(relativeStart: Int): Relative = Relative(Ior.left(relativeStart))

    def fromStart(relativeEnd: Int): Relative = Relative(Ior.right(relativeEnd))

    def from(
      relativeStart: Int,
      relativeEnd: Int,
    ): Relative = Relative(Ior.both(relativeStart, relativeEnd))
  }

  /**
    * A [[Relative]] slice applied to a known size, with a calculated and fixed start and end.
    *
    * This could contain the total size as well, but I didn't see a need for it yet.
    */
  final case class Absolute private[SliceRange] (
    start: Int,
    end: Int,
    relative: Relative,
  ) {

    def this(
      start: Int,
      end: Int,
    ) = this(start, end, Relative(Ior.Both(start, end)))

    val toRange: Range = Range.inclusive(start, end)

    def contains(index: Int): Boolean = toRange.contains(index)
  }

  object Absolute {

    def apply(
      start: Int,
      end: Int,
    ): Absolute = new Absolute(start, end)
  }

  final class Syntax(private val num: Int) extends AnyVal {

    def fromEnd: Relative = Relative.fromEnd(num)

    def fromStart: Relative = Relative.fromStart(num)

    def <->(relativeEnd: Int): Relative = Relative.from(num, relativeEnd)

    def <->(end: End.type): Relative = Relative.fromEnd(num)
  }

  final case object End
}

trait SliceRangeSyntax {

  final val End = SliceRange.End

  implicit def range(num: Int): SliceRange.Syntax = new SliceRange.Syntax(num)
}
