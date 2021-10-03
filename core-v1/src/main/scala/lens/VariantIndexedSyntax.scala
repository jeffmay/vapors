package com.rallyhealth.vapors.v1

package lens

import cats.Semigroup
import cats.data.NonEmptySet

object VariantIndexedSyntax extends VariantIndexedSyntax

trait VariantIndexedSyntax {

  implicit def indexedOps[C](collection: C): VariantIndexedOps[C] = new VariantIndexedOps(collection)
}

final class VariantIndexedOps[C](private val collection: C) extends AnyVal {

  def get[K, V](key: K)(implicit indexed: VariantIndexed[C, K, V]): V = indexed.get(collection)(key)

  def filterKeys[K, V](
    keys: NonEmptySet[K],
  )(implicit
    indexed: VariantIndexed[C, K, V],
    semigroupM: Semigroup[V],
  ): V = keys.toNonEmptyList.map(get(_)).reduce
}
