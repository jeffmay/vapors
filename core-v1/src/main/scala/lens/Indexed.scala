package com.rallyhealth.vapors.v1

package lens

import cats.Foldable

/**
  * Defines how to get an instance of [[V]] from a collection [[C]], keyed by [[K]].
  */
trait Indexed[C, K, V] {

  /**
    * Get a value from the given container by the given key.
    */
  def get(container: C)(key: K): V
}

object Indexed extends LowPriorityIndexed {

  @inline final def apply[C, K, V](implicit M: Indexed[C, K, V]): Indexed[C, K, V] = M

  type MapLike[M[_, _], K, V] = Indexed[M[K, V], K, Option[V]]
  type MapOf[K, V] = MapLike[Map, K, V]
  type StringMap[V] = MapOf[String, V]
  type IntMap[V] = MapOf[Int, V]

  implicit def indexedMap[K, V]: Indexed[Map[K, V], K, Option[V]] = {
    new Indexed[Map[K, V], K, Option[V]] {
      override def get(container: Map[K, V])(key: K): Option[V] = container.get(key)
    }
  }

  implicit def indexedTuple[T <: NonEmptyTuple, N <: Int]: Indexed[T, N, Tuple.Elem[T, N]] = {
    new Indexed[T, N, Tuple.Elem[T, N]] {
      override def get(container: T)(key: N): Tuple.Elem[T, key.type] = container.apply(key)
    }
  }

  final class FromFoldable[F[_] : Foldable, K : Numeric, V] extends Indexed[F[V], K, Option[V]] {
    override def get(container: F[V])(key: K): Option[V] = Foldable[F].get(container)(Numeric[K].toLong(key))
  }
}

private[vapors] trait LowPriorityIndexed {

  implicit def indexedFoldableFromInt[F[_] : Foldable, V]: Indexed[F[V], Int, Option[V]] =
    new Indexed.FromFoldable[F, Int, V]

  implicit def indexedFoldableFromLong[F[_] : Foldable, V]: Indexed[F[V], Long, Option[V]] =
    new Indexed.FromFoldable[F, Long, V]

}
