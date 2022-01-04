package com.rallyhealth.vapors.v1

package algebra

import data.{Extract, Window}
import dsl.WrapSelected
import lens.DataPath

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

  def asProductType[I <: HList, O](implicit gen: Generic.Aux[O, I]): ExprConverter[I, O] =
    new Impl("asProduct", gen.from)

  def asWrappedProductType[W[+_] : Extract, I <: HList : OP, O : OP, OP[_]](
    implicit
    gen: Generic.Aux[O, I],
    wrapSelected: WrapSelected[W, OP],
  ): ExprConverter[W[I], W[O]] =
    new Impl("asProductWrapped", wi => wrapSelected.wrapSelected(wi, DataPath.empty, gen.from(Extract[W].extract(wi))))

  def asTuple[I <: HList, O](implicit tupler: Tupler.Aux[I, O]): ExprConverter[I, O] =
    new Impl("asTuple", tupler.apply)
}
