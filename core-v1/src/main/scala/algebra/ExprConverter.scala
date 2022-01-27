package com.rallyhealth.vapors.v1

package algebra

import data.{Extract, Window}
import dsl.WrapSelected
import lens.DataPath

import shapeless3.deriving.{Id, K0}

import scala.deriving.Mirror

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

  def asProductType[I <: Tuple](implicit gen: Mirror.Product): ExprConverter[I, gen.MirroredMonoType] =
    new Impl("asProduct", gen.fromProduct)

  def asWrappedProductType[W[+_] : Extract, I <: Tuple : OP, O : OP, OP[_]](
    implicit
    gen: K0.Generic[O],
    wrapSelected: WrapSelected[W, OP],
  ): ExprConverter[W[I], W[O]] =
    new Impl(
      "asProductWrapped",
      wi => wrapSelected.wrapSelected(wi, DataPath.empty, gen.fromRepr(Extract[W].extract(wi))),
    )

}
