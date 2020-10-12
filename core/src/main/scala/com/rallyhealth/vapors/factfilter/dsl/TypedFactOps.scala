package com.rallyhealth.vapors.factfilter.dsl

import com.rallyhealth.vapors.factfilter.data.{FactType, FactTypeSet}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

private[dsl] trait TypedFactOps {

  def withType[T >: U, U : ClassTag : TypeTag](factType: FactType[U]): WhereBuilder[T, U] = {
    new WhereBuilder[T, U](FactTypeSet.of(factType))
  }

  def withTypeIn[T >: U, U : ClassTag : TypeTag](factTypeSet: FactTypeSet[U]): WhereBuilder[T, U] = {
    new WhereBuilder[T, U](factTypeSet)
  }

}
