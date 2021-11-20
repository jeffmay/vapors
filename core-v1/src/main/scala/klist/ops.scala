package com.rallyhealth.vapors.v1

package klist

import shapeless.{::, HList}

object ops {

  trait IsKCons[KL <: KList[F, _], F[_]] {
    type H
    type T <: HList

    def head(l: KL): F[H]
    def tail(l: KL): KList[F, T]

    def cons(
      h: F[H],
      t: KList[F, T],
    ): KL
  }

  object IsKCons {

    type Aux[KL <: KList[F, _], F[_], H0, T0 <: HList] = IsKCons[KL, F] {
      type H = H0
      type T = T0
    }

    implicit def isKCons[F[_], H0, T0 <: HList]: IsKCons.Aux[KCons[F, H0, T0], F, H0, T0] =
      new IsKCons[KCons[F, H0, T0], F] {
        override type H = H0
        override type T = T0

        override def head(kl: KCons[F, H0, T0]): F[H] = kl.head
        override def tail(kl: KCons[F, H0, T0]): KList[F, T] = kl.tail

        override def cons(
          h: F[H0],
          t: KList[F, T],
        ): KCons[F, H0, T0] = KCons(h, t)
      }
  }
}
