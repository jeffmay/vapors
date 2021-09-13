package com.rallyhealth.vapors.v1

package lens

import cats.Semigroup
import cats.data.NonEmptySet

object IndexedSyntax extends IndexedSyntax

trait IndexedSyntax {

  implicit def indexedOps[C](collection: C): IndexedOps[C] = new IndexedOps(collection)
}

final class IndexedOps[C](private val collection: C) extends AnyVal {

  def get[K, V](key: K)(implicit indexed: Indexed[C, K, V]): V = indexed.get(collection)(key)

  def filterKeys[K, V](
    keys: NonEmptySet[K],
  )(implicit
    indexed: Indexed[C, K, V],
    semigroupM: Semigroup[V],
  ): V = keys.toNonEmptyList.map(get(_)).reduce
}
