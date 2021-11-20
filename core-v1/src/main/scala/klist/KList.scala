package com.rallyhealth.vapors.v1

package klist

import cats.~>
import shapeless.{::, HList, HNil}

// TODO: Figure out how to replace this with UnaryTCConstraint (and whether that is a good idea)
//       https://stackoverflow.com/questions/64622828/shapeless-mapping-an-natural-transformation-over-a-klist
sealed trait KList[+F[_], HL <: HList] {

  def :^:[N[X] >: F[X], A](g: N[A]): KCons[N, A, HL] = KCons(g, this)

  def transform[M[a] >: F[a], N[_]](fn: M ~> N): KList[N, HL]
}

// TODO: Should I require variance here?
final case class KCons[+F[_], H, T <: HList](
  head: F[H],
  tail: KList[F, T],
) extends KList[F, H :: T] {

  override def transform[M[a] >: F[a], N[_]](fn: M ~> N): KList[N, H :: T] =
    KCons(fn(head), tail.transform(fn))
}

sealed class KNil extends KList[Nothing, HNil] {

  override final def transform[M[_], N[_]](fn: M ~> N): KList[N, HNil] = this
}

object KNil extends KNil

object KList {

  implicit final class Ops[KL <: KList[Any, HL], HL <: HList](private val kl: KL) extends AnyVal {

    def toHList(implicit thl: KToHList[KL]): thl.HL = thl(kl)
  }
}
