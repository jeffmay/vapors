package com.rallyhealth.vapors.v2
package debug

/**
 * Defines a typeclass to serialize each output type value to a string.
 *
 * @see [[Show]] for more details.
 * @tparam V the expression output type parameter.
 */
trait HasShow[-V] {

  def show: Show[V]
}

object HasShow extends LowPriorityHasShow {

  inline final def apply[V : HasShow]: HasShow[V] = summon

  inline final def unapply[V](has: HasShow[V]): Some[Show[V]] = Some(has.show)

  private final case class Impl[-V](show: Show[V]) extends HasShow[V] {
    override def productPrefix: String = "HasShow"
  }

  inline given show[V : Show]: HasShow[V] = Impl(Show[V])
}

private[debug] sealed abstract class LowPriorityHasShow {

  /**
   * There is no [[Show]] instance available for this type, so just use `.toString`
   *
   * Similar to [[dsl.NoOP.~]], this method name is short because it will show up in a lot of places and
   * it means that you can effectively ignore this parameter.
   */
  inline implicit def ~[V]: HasShow[V] = HasShowToString
}

case object HasShowToString extends HasShow[Any] {
  override def show: Show[Any] = ShowToString
}
