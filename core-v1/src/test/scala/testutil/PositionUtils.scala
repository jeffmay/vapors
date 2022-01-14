package com.rallyhealth.vapors.v1

package testutil

import org.scalactic.source.Position

object PositionUtils {

  def stringify(location: Position): String = s"${location.fileName}:${location.lineNumber}"
}
