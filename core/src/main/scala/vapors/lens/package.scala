package com.rallyhealth

package vapors

package object lens {

  type DataPath = v1.lens.DataPath
  final val DataPath = v1.lens.DataPath

  type Indexed[C, K, V] = v1.lens.Indexed[C, K, V]
  final val Indexed = v1.lens.Indexed

  type ValidDataPathKey[K] = v1.lens.ValidDataPathKey[K]
  final val ValidDataPathKey = v1.lens.ValidDataPathKey
}
