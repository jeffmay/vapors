package com.rallyhealth.vapors.v1

package lens

import cats.data.{NonEmptyList, NonEmptySeq, NonEmptyVector}

import scala.collection.Factory

/**
  * Defines the ability to map over elements of [[A]] to produce a value of [[Some]] new type [[B]],
  * as well as the ability to filter the element out by returning [[None]]. This also allows changing
  * the collection type in the process.
  *
  * For example, if you filter elements from a non-empty collection, you might end up with an empty
  * collection, so calling .filter or .collect on a [[NonEmptyList]] would produce a [[List]].
  *
  * @note if you set type [[A]] to the same as type [[B]], then this operation becomes a filter.
  *
  * @see [[CollectInto.Filter]]
  *
  * @tparam C the collection type
  * @tparam A the original element type
  * @tparam B the new element type
  */
trait CollectInto[C[_], A, B] {
  type Out[_]

  def filter(
    collection: C[A],
    collector: A => Boolean,
  )(implicit
    ev: A <:< B,
  ): Out[B]

  def collectSome(
    collection: C[A],
    collector: A => Option[B],
  ): Out[B] = collectSomeWithIndex(collection, (a, _) => collector(a))

  def collectSomeWithIndex(
    collection: C[A],
    collector: (A, Int) => Option[B],
  ): Out[B]
}

object CollectInto {
  type Aux[C[_], A, B, D[_]] = CollectInto[C, A, B] { type Out[a] = D[a] }
  type Filter[C[_], A, D[_]] = CollectInto[C, A, A] { type Out[a] = D[a] }

  private final class CollectUsingFactory[C[_], A, B, D[_]](
    asIterableOnce: C[A] => IterableOnce[A],
  )(implicit
    factory: Factory[B, D[B]],
  ) extends CollectInto[C, A, B] {

    override type Out[b] = D[b]

    override def filter(
      collection: C[A],
      predicate: A => Boolean,
    )(implicit
      ev: A <:< B,
    ): D[B] = {
      factory.fromSpecific(asIterableOnce(collection).iterator.collect {
        case a if predicate(a) => ev(a)
      })
    }

    override def collectSome(
      collection: C[A],
      collector: A => Option[B],
    ): D[B] = factory.fromSpecific(asIterableOnce(collection).iterator.collect(Function.unlift(collector)))

    override def collectSomeWithIndex(
      collection: C[A],
      collector: (A, Int) => Option[B],
    ): D[B] =
      factory.fromSpecific(asIterableOnce(collection).iterator.zipWithIndex.collect(Function.unlift(collector.tupled)))
  }

  implicit def iterable[C[a] <: IterableOnce[a], A, B](
    implicit
    factory: Factory[B, C[B]],
  ): CollectInto.Aux[C, A, B, C] =
    new CollectUsingFactory[C, A, B, C](identity)

  implicit def nonEmptySeq[A, B]: CollectInto.Aux[NonEmptySeq, A, B, Seq] =
    new CollectUsingFactory[NonEmptySeq, A, B, Seq](_.toSeq)

  implicit def nonEmptyList[A, B]: CollectInto.Aux[NonEmptyList, A, B, List] =
    new CollectUsingFactory[NonEmptyList, A, B, List](_.toList)

  implicit def nonEmptyVector[A, B]: CollectInto.Aux[NonEmptyVector, A, B, Vector] =
    new CollectUsingFactory[NonEmptyVector, A, B, Vector](_.toVector)

  private final class CollectIntoOption[A, B] extends CollectInto[Option, A, B] {

    override type Out[a] = Option[a]

    override def filter(
      collection: Option[A],
      collector: A => Boolean,
    )(implicit
      ev: A <:< B,
    ): Option[B] =
      collection.collect {
        case a if collector(a) => ev(a)
      }

    override def collectSomeWithIndex(
      collection: Option[A],
      collector: (A, Int) => Option[B],
    ): Option[B] =
      collection.collect(Function.unlift(collector(_, 0)))
  }

  implicit def option[A, B]: CollectInto.Aux[Option, A, B, Option] = new CollectIntoOption[A, B]

}
