package com.rallyhealth.vapors.v1

package algebra

import cats.data.{NonEmptyList, NonEmptySeq, NonEmptyVector}

/**
  * Defines how to compare collection of type [[C]] to a numeric integral value of type [[N]] using the
  * [[SizeComparison]] operator.
  *
  * @tparam C the concrete type of collection
  * @tparam N the concrete numeric integral type (or any type that is viewable as an integer)
  * @tparam B the boolean-like type (or any type viewable as a boolean) that is returned
  */
trait SizeComparable[-C, -N, +B] { outer =>

  def sizeCompare(
    collection: C,
    comparison: SizeComparison,
    comparedTo: N,
  ): B

  def contramapCollection[I](fn: I => C): SizeComparable[I, N, B] = { (collection, comparison, comparedTo) =>
    outer.sizeCompare(fn(collection), comparison, comparedTo)
  }

  def contramapComparedTo[I](fn: I => N): SizeComparable[C, I, B] = { (collection, comparison, comparedTo) =>
    outer.sizeCompare(collection, comparison, fn(comparedTo))
  }

  def map[O](fn: B => O): SizeComparable[C, N, O] = { (collection, comparison, comparedTo) =>
    fn(outer.sizeCompare(collection, comparison, comparedTo))
  }
}

object SizeComparable {

  private final class FromIterableToBoolean[-C](asIterable: C => Iterable[Any])
    extends SizeComparable[C, Int, Boolean] {
    override def sizeCompare(
      collection: C,
      comparison: SizeComparison,
      comparedTo: Int,
    ): Boolean = {
      val iterable = asIterable(collection)
      val result = comparison match {
        case SizeComparison.=== => iterable.sizeIs == comparedTo
        case SizeComparison.< => iterable.sizeIs < comparedTo
        case SizeComparison.<= => iterable.sizeIs <= comparedTo
        case SizeComparison.> => iterable.sizeIs > comparedTo
        case SizeComparison.>= => iterable.sizeIs >= comparedTo
      }
      result
    }
  }

  implicit val iterableSizeCompare: SizeComparable[Iterable[Any], Int, Boolean] =
    new FromIterableToBoolean(identity)

  implicit val optionSizeCompare: SizeComparable[Option[Any], Int, Boolean] =
    new FromIterableToBoolean(_.toIterable)

  implicit val nonEmptySeqSizeCompare: SizeComparable[NonEmptySeq[Any], Int, Boolean] =
    new FromIterableToBoolean(_.toSeq)

  implicit val nonEmptyListSizeCompare: SizeComparable[NonEmptyList[Any], Int, Boolean] =
    new FromIterableToBoolean(_.toList)

  implicit val nonEmptyVectorSizeCompare: SizeComparable[NonEmptyVector[Any], Int, Boolean] =
    new FromIterableToBoolean(_.toVector)
}

sealed abstract class SizeComparison(val symbol: String)

object SizeComparison {
  final case object === extends SizeComparison("===")
  final case object < extends SizeComparison("<")
  final case object <= extends SizeComparison("<=")
  final case object > extends SizeComparison(">")
  final case object >= extends SizeComparison(">=")
}
