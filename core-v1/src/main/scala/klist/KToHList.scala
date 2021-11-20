package com.rallyhealth.vapors.v1

package klist

import klist.ops.IsKCons

import shapeless.{::, HList, HNil}

sealed trait KToHList[KL <: KList[Any, _]] {
  type HL <: HList

  def apply(kl: KL): HL
}

object Example {

  // Doesn't work... :(
//  val nilHL = KNil.toHList
//  val one = List(1, 2, 3) :^: KNil
//  val oneHL = one.toHList
//
//  val two = List("one", "two", "three") :^: one
//  val twoHL = two.toHList
}

object KToHList extends LowPriorityKToHList {
  type Aux[KL <: KList[Any, _], O <: HList] = KToHList[KL] { type HL = O }

  implicit def toHNil[KL <: KList[Any, HNil]]: KToHList.Aux[KL, HNil] =
    new KToHList[KL] {
      override type HL = HNil
      override final def apply(kl: KL): HNil = HNil
    }

  implicit def toHLast[KL <: KList[F, H :: HNil], F[+_], H](
    implicit
    isHCons: IsKCons.Aux[KL, F, H, HNil],
  ): KToHList[KL] =
    new KToHList[KL] {
      override type HL = F[H] :: HNil
      override def apply(kl: KL): F[H] :: HNil = {
        isHCons.head(kl) :: HNil
      }
    }

  implicit def toHCons[KL <: KList[F, H :: T], F[+_], H, T <: HList](
    implicit
    isHCons: IsKCons.Aux[KL, F, H, T],
    toHList: KToHList[KList[F, T]],
  ): KToHList.Aux[KL, F[H] :: toHList.HL] =
    new KToHList[KL] {
      override type HL = F[H] :: toHList.HL
      override def apply(kl: KL): F[H] :: toHList.HL = {
        isHCons.head(kl) :: toHList(isHCons.tail(kl))
      }
    }

//  implicit def toHList[F[+_], H, T <: HList](
//    implicit
//    toHList: KToHList[KList[F, T]],
//  ): KToHList.Aux[KCons[F, H, T], F[H] :: toHList.HL] =
//    new KToHList[KCons[F, H, T]] {
//      override type HL = F[H] :: toHList.HL
//      override def apply(kl: KCons[F, H, T]): F[H] :: toHList.HL = {
//        kl.head :: toHList(kl.tail)
//      }
//    }

//  implicit def hlast[KL <: KList[F, H :: HNil], F[_], H](
//    implicit
//    isKCons: ops.IsKCons[KL, F],
//  ): KToHList.Aux[KL, F[H] :: HNil] =
//    new KToHList[KList[F, H :: HNil]] {
//      override type Out = F[H] :: HNil
//      override def apply(kl: KL): F[H] :: HNil = isKCons.head(kl) :: HNil
//    }

//  implicit def toHLast[F[_], H]: KToHList.Aux[KList[F, H :: HNil], F[H] :: HNil] =
//    new KToHList[KList[F, H :: HNil]] {
//      override type HL = F[H] :: HNil
//      override final def apply(kl: KList[F, H :: HNil]): F[H] :: HNil = kl.head :: HNil
//    }

}

sealed trait LowPriorityKToHList {

//  implicit def toHCons[F[_], H, T <: HList](
//    implicit
//    tailToHList: KToHList[KList[F, T]],
//  ): KToHList.Aux[KCons[F, H, T], F[H] :: tailToHList.HL] =
//    new KToHList[KCons[F, H, T]] {
//      override type HL = F[H] :: tailToHList.HL
//      override def apply(kl: KCons[F, H, T]): F[H] :: tailToHList.HL = {
//        kl.head :: tailToHList(kl.tail)
//      }
//    }

//  implicit def toKCons[F[_], H, T <: HList, KT <: KList[F, T]](
//    implicit
//    tailToHList: KToHList[KT],
//  ): KToHList.Aux[KCons[F, H, T], F[H] :: tailToHList.HL] =
//    new KToHList[KCons[F, H, T]] {
//      override type HL = F[H] :: tailToHList.HL
//      override def apply(kl: KCons[F, H, T]): F[H] :: tailToHList.HL = {
////        val h = isKCons.head(kl)
////        val t = isKCons.tail(kl)
//        kl.head :: tailToHList(kl.tail)
//        //        kl.head :: tailToHList(kl.tail)
//      }
//    }

//  implicit def toKCons[KL <: KList[F, H :: T], F[_], H, T <: HList, KT <: KList[F, T]](
//    implicit
//    isKCons: IsKCons.Aux[KL, F, H, T],
//    tailToHList: KToHList[KT],
//  ): KToHList.Aux[KL, F[isKCons.H] :: tailToHList.HL] =
//    new KToHList[KL] {
//      override type HL = F[isKCons.H] :: tailToHList.HL
//      override def apply(kl: KL): F[isKCons.H] :: tailToHList.HL = {
//        val h = isKCons.head(kl)
//        val t = isKCons.tail(kl)
//        tailToHList(t)
////        kl.head :: tailToHList(kl.tail)
//      }
//    }

//  implicit def toHCons[KL <: KCons[F, H, T], F[_], H, T <: HList](
//    implicit
//    tailToHList: KToHList[KList[F, T]],
//  ): KToHList.Aux[KL, F[H] :: tailToHList.HL] =
//    new KToHList[KL] {
//      override type HL = F[H] :: tailToHList.HL
//      override def apply(kl: KL): F[H] :: tailToHList.HL = {
//        kl.head :: tailToHList(kl.tail)
//      }
//    }
}
