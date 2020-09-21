package com.rallyhealth.vapors.core.puml

sealed trait Length

object Length {
  final case object Short extends Length
  final case object Medium extends Length
  final case object Long extends Length
}
