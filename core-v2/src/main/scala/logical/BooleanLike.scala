package com.rallyhealth.vapors.v2
package logical

import optical.Extracting

trait BooleanLike[-A] {
  def toBoolean(source: A): Boolean
}

object BooleanLike {

  class ByExtracting[-A : Extracting[Boolean]] extends BooleanLike[A] {
    import Extracting.extract
    final override def toBoolean(source: A): Boolean = source.extract[Boolean]
  }
}