package com.rallyhealth.vapors.v1

package dsl

object standard extends BuildExprDsl[NoOP] with StandardRunDsl[NoOP] {

  final object withSourceInfo extends BuildExprDsl[SourceCodeInfo] with StandardRunDsl[SourceCodeInfo]
}
