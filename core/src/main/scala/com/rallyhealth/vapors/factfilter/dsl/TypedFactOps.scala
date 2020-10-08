package com.rallyhealth.vapors.factfilter.dsl

import com.rallyhealth.vapors.factfilter.data.{FactType, FactTypeSet}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

private[dsl] trait TypedFactOps {

  def withFactsOfType[T >: U, U : ClassTag : TypeTag](factType: FactType[U]): WhereFactsExpBuilder[T, U] = {
    new WhereFactsExpBuilder[T, U](FactTypeSet.of(factType))
  }

  def withFactsOfTypeIn[T >: U, U : ClassTag : TypeTag](factTypeSet: FactTypeSet[U]): WhereFactsExpBuilder[T, U] = {
    new WhereFactsExpBuilder[T, U](factTypeSet)
  }
}
