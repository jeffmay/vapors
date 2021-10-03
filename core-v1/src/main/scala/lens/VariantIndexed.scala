package com.rallyhealth.vapors.v1

package lens

import cats.Foldable
import shapeless.ops.{hlist, tuple}
import shapeless.{HList, Nat}

/**
  * Defines how to get an instance of [[V]] from a collection [[C]], keyed by [[K]].
  */
trait VariantIndexed[-C, -K, +V] {

  /**
    * Get a value from the given container by the given key.
    */
  def get(container: C)(key: K): V
}

object VariantIndexed extends LowPriorityVariantIndexed {

  @inline final def apply[C, K, V](implicit M: Indexed[C, K, V]): Indexed[C, K, V] = M

  type MapLike[M[_, _], K, V] = VariantIndexed[M[K, V], K, Option[V]]
  type MapOf[K, V] = MapLike[Map, K, V]
  type StringMap[V] = MapOf[String, V]
  type IntMap[V] = MapOf[Int, V]

  implicit def indexedMap[K, V]: VariantIndexed[Map[K, V], K, Option[V]] = {
    new VariantIndexed[Map[K, V], K, Option[V]] {
      override def get(container: Map[K, V])(key: K): Option[V] = container.get(key)
    }
  }

  implicit def indexedHList[L <: HList, N <: Nat, V](implicit at: hlist.At.Aux[L, N, V]): VariantIndexed[L, N, V] = {
    new VariantIndexed[L, N, V] {
      override def get(container: L)(key: N): V = at(container)
    }
  }

  implicit def indexedTuple[T <: Product, N <: Nat, V](implicit at: tuple.At.Aux[T, N, V]): VariantIndexed[T, N, V] = {
    new VariantIndexed[T, N, V] {
      override def get(container: T)(key: N): V = at(container)
    }
  }

  final class FromFoldable[F[_] : Foldable, K : Numeric, V] extends VariantIndexed[F[V], K, Option[V]] {
    override def get(container: F[V])(key: K): Option[V] = Foldable[F].get(container)(Numeric[K].toLong(key))
  }
}

private[vapors] trait LowPriorityVariantIndexed {

  implicit def indexedFoldableFromInt[F[_] : Foldable, V]: VariantIndexed[F[V], Int, Option[V]] =
    new VariantIndexed.FromFoldable[F, Int, V]

  implicit def indexedFoldableFromLong[F[_] : Foldable, V]: VariantIndexed[F[V], Long, Option[V]] =
    new VariantIndexed.FromFoldable[F, Long, V]

}
