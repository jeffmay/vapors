package com.rallyhealth.vapors.v1

package debug

import cats.Show

trait HasShow[V] {

  def show: Show[V]
}
