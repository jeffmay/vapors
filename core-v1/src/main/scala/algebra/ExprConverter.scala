package com.rallyhealth.vapors.v1

package algebra

import data.Window

import cats.Functor
import shapeless.ops.hlist.Tupler
import shapeless.{Generic, HList}

/**
  * A serializable description of a total conversion function from one type to another.
  */
sealed trait ExprConverter[-I, +O] extends (I => O) {

  /**
    * The type of conversion applied.
    */
  def conversionType: String

  /**
    * Apply the conversion.
    *
    * @note this should never throw an exception.
    */
  override def apply(inputValue: I): O
}

object ExprConverter {

  // TODO: Capture type information about O for debugging?
  private final class Impl[-I, +O](
    override val conversionType: String,
    convert: I => O,
  ) extends ExprConverter[I, O] {
    override def apply(in: I): O = convert(in)
    override val toString: String = s"ExprConverter.$conversionType"
  }

  def asHListIdentity[O <: HList]: ExprConverter[O, O] = new Impl("asHList", identity)

  def asProductType[I <: HList, O](implicit gen: Generic.Aux[O, I]): ExprConverter[I, O] =
    new Impl("asProduct", gen.from)

  def asWrappedProductType[W[+_] : Functor, I <: HList, O](implicit gen: Generic.Aux[O, I]): ExprConverter[W[I], W[O]] =
    new Impl("asProductWrapped", Functor[W].map(_)(gen.from))

  def asTuple[I <: HList, O](implicit tupler: Tupler.Aux[I, O]): ExprConverter[I, O] =
    new Impl("asTuple", tupler.apply)

  // TODO: Take more metadata about the bounds that this value is being converted to
  def asWindow[O](buildWindow: O => Window[O]): ExprConverter[O, Window[O]] =
    new Impl("asWindow", buildWindow)
}
