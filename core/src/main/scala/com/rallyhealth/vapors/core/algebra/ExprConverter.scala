package com.rallyhealth.vapors.core.algebra

import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

sealed trait ExprConverter[L, R] extends (L => R) {
  def conversionType: String
  override def apply(inputValue: L): R
}

object ExprConverter {

  // TODO: Capture type information about R for debugging?
  private final class Impl[L, R](
    convert: L => R,
    override val conversionType: String,
  ) extends ExprConverter[L, R] {
    override def apply(in: L): R = convert(in)
  }

  def asHListIdentity[R <: HList]: ExprConverter[R, R] = new Impl(identity, "asHList")

  def asProductType[L <: HList, R](implicit gen: Generic.Aux[R, L]): ExprConverter[L, R] =
    new Impl(gen.from, "asProduct")

  def asTuple[L <: HList, R](implicit tupler: Tupler.Aux[L, R]): ExprConverter[L, R] =
    new Impl(tupler.apply, "asTuple")
}
