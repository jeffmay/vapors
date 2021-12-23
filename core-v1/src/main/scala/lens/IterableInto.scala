package com.rallyhealth.vapors.v1

package lens

import cats.data.{NonEmptyList, NonEmptySeq, NonEmptyVector}

import scala.collection.{mutable, Factory}

trait IterableInto[C[_], -A] {
  type Out

  def fromIterable(iterable: IterableOnce[A]): Out

  def intoFactory: Factory[A, Out]
}

object IterableInto {

  type Aux[C[_], -A, O] = IterableInto[C, A] { type Out = O }

  implicit def convertFactory[C[_], A](factory: Factory[A, C[A]]): IterableInto.Aux[C, A, C[A]] = intoFactory(factory)

  private final class IterableIntoFactory[C[_], A](
    implicit
    sourceFactory: Factory[A, C[A]],
  ) extends IterableInto[C, A] {
    override type Out = C[A]
    override def fromIterable(iterable: IterableOnce[A]): C[A] = sourceFactory.fromSpecific(iterable)
    override def intoFactory: Factory[A, C[A]] = sourceFactory
  }

  implicit def intoFactory[C[_], A](implicit factory: Factory[A, C[A]]): IterableInto.Aux[C, A, C[A]] =
    new IterableIntoFactory[C, A]

  private final class IterableIntoNonEmpty[C[_], S[_], A](
    fromSource: S[A] => Option[C[A]],
  )(implicit
    sourceFactory: Factory[A, S[A]],
  ) extends IterableInto[C, A] {
    override type Out = Option[C[A]]
    override def fromIterable(iterable: IterableOnce[A]): Option[C[A]] = intoFactory.fromSpecific(iterable)
    override val intoFactory: Factory[A, Option[C[A]]] = new IterableIntoNonEmptyFactory(fromSource)
  }

  private final class IterableIntoNonEmptyFactory[C[_], S[_], A](
    fromSource: S[A] => Option[C[A]],
  )(implicit
    sourceFactory: Factory[A, S[A]],
  ) extends Factory[A, Option[C[A]]] {
    override def fromSpecific(it: IterableOnce[A]): Option[C[A]] = fromSource(sourceFactory.fromSpecific(it))
    override def newBuilder: mutable.Builder[A, Option[C[A]]] = sourceFactory.newBuilder.mapResult(fromSource)
  }

  implicit def intoNonEmptySeq[A]: IterableInto.Aux[NonEmptySeq, A, Option[NonEmptySeq[A]]] =
    new IterableIntoNonEmpty[NonEmptySeq, Seq, A](NonEmptySeq.fromSeq)

  implicit def intoNonEmptyList[A]: IterableInto.Aux[NonEmptyList, A, Option[NonEmptyList[A]]] =
    new IterableIntoNonEmpty[NonEmptyList, List, A](NonEmptyList.fromList)

  implicit def intoNonEmptyVector[A]: IterableInto.Aux[NonEmptyVector, A, Option[NonEmptyVector[A]]] =
    new IterableIntoNonEmpty[NonEmptyVector, Vector, A](NonEmptyVector.fromVector)
}
