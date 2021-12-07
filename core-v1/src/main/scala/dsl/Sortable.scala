package com.rallyhealth.vapors.v1

package dsl

import cats.Order
import cats.data._

import scala.collection.Factory
import scala.reflect.ClassTag

/**
  * Similar to [[Order]], except that it allows you to define the operation over the container type [[C]],
  * rather than requiring separate implicits to convert the collection to a [[Seq]], then call `.sorted`
  * with an [[Order]] of [[A]], then convert back to [[C]] with a [[Factory]].
  *
  * @tparam C the container type inside which to sort the elements
  * @tparam A the element type that can be ordered
  */
trait Sortable[C[_], A] {
  def sort(ca: C[A]): C[A]
}

object Sortable {

  private final class ViaArray[C[_], A : ClassTag : Order](
    fromArray: Array[A] => C[A],
    toArray: C[A] => Array[A],
  ) extends Sortable[C, A] {
    override def sort(ca: C[A]): C[A] = {
      val arr = toArray(ca)
      arr.sortInPlace()(Order[A].toOrdering)
      fromArray(arr)
    }
  }

  implicit def seq[C[a] <: Seq[a], A : ClassTag : Order](implicit factory: Factory[A, C[A]]): Sortable[C, A] =
    new ViaArray(factory.fromSpecific(_), _.toArray)

  implicit def nonEmptyChain[A : ClassTag : Order]: Sortable[NonEmptyChain, A] =
    new ViaArray[NonEmptyChain, A](ca => NonEmptyChain.fromChainUnsafe(Chain.fromSeq(ca)), _.toChain.iterator.toArray)

  implicit def nonEmptyList[A : ClassTag : Order]: Sortable[NonEmptyList, A] =
    new ViaArray(ca => NonEmptyList.fromListUnsafe(ca.toList), _.toList.toArray)

  implicit def nonEmptySeq[A : ClassTag : Order]: Sortable[NonEmptySeq, A] =
    new ViaArray(ca => NonEmptySeq.fromSeqUnsafe(ca), _.toSeq.toArray)

  implicit def nonEmptyVector[A : ClassTag : Order]: Sortable[NonEmptyVector, A] =
    new ViaArray(ca => NonEmptyVector.fromVectorUnsafe(ca.toVector), _.toVector.toArray)
}
