package com.rallyhealth.vapors.v1

import debug.HasSourceCodeInfo

package object dsl {

  final type NoOP[_] = DummyImplicit

  final type SourceCodeInfo[_] = HasSourceCodeInfo
}
