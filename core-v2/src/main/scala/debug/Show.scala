package com.rallyhealth.vapors.v2
package debug

import zio.prelude.*

trait Show[-V] {
  def show(value: V): String
}

object Show extends LowPriorityShow {
  inline final def apply[V : Show]: Show[V] = summon
}

private[debug] sealed trait LowPriorityShow {
  self: Show.type =>

  inline given ~[A]: Show[A] = ShowToString
}

case object ShowToString extends Show[Any] {
  override def show(value: Any): String = value.toString
}
