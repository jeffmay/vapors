package com.rallyhealth.vapors.v1.dsl

import shapeless.{::, HNil}

trait WrapArityMethods {
  self: DslTypes =>

  def wrap[I, A1, A2](
    x1: I ~:> A1,
    x2: I ~:> A2,
  ): XHL[I, A1 :: A2 :: HNil] =
    x1 :: x2 :: XHNil

  def wrap[I, A1, A2, A3](
    x1: I ~:> A1,
    x2: I ~:> A2,
    x3: I ~:> A3,
  ): XHL[I, A1 :: A2 :: A3 :: HNil] =
    x1 :: x2 :: x3 :: XHNil

  def wrap[I, A1, A2, A3, A4](
    x1: I ~:> A1,
    x2: I ~:> A2,
    x3: I ~:> A3,
    x4: I ~:> A4,
  ): XHL[I, A1 :: A2 :: A3 :: A4 :: HNil] =
    x1 :: x2 :: x3 :: x4 :: XHNil

  def wrap[I, A1, A2, A3, A4, A5](
    x1: I ~:> A1,
    x2: I ~:> A2,
    x3: I ~:> A3,
    x4: I ~:> A4,
    x5: I ~:> A5,
  ): XHL[I, A1 :: A2 :: A3 :: A4 :: A5 :: HNil] =
    x1 :: x2 :: x3 :: x4 :: x5 :: XHNil

  def wrap[I, A1, A2, A3, A4, A5, A6](
    x1: I ~:> A1,
    x2: I ~:> A2,
    x3: I ~:> A3,
    x4: I ~:> A4,
    x5: I ~:> A5,
    x6: I ~:> A6,
  ): XHL[I, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: HNil] =
    x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: XHNil

  def wrap[I, A1, A2, A3, A4, A5, A6, A7](
    x1: I ~:> A1,
    x2: I ~:> A2,
    x3: I ~:> A3,
    x4: I ~:> A4,
    x5: I ~:> A5,
    x6: I ~:> A6,
    x7: I ~:> A7,
  ): XHL[I, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: HNil] =
    x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: XHNil

  def wrap[I, A1, A2, A3, A4, A5, A6, A7, A8](
    x1: I ~:> A1,
    x2: I ~:> A2,
    x3: I ~:> A3,
    x4: I ~:> A4,
    x5: I ~:> A5,
    x6: I ~:> A6,
    x7: I ~:> A7,
    x8: I ~:> A8,
  ): XHL[I, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: HNil] =
    x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: XHNil

  def wrap[I, A1, A2, A3, A4, A5, A6, A7, A8, A9](
    x1: I ~:> A1,
    x2: I ~:> A2,
    x3: I ~:> A3,
    x4: I ~:> A4,
    x5: I ~:> A5,
    x6: I ~:> A6,
    x7: I ~:> A7,
    x8: I ~:> A8,
    x9: I ~:> A9,
  ): XHL[I, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: HNil] =
    x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: XHNil
}
