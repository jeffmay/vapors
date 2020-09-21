package com.rallyhealth.vapors.core.puml

sealed trait Direction

object Direction {
  final case object Up extends Direction
  final case object Down extends Direction
  final case object Left extends Direction
  final case object Right extends Direction
}
