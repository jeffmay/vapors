package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.BuildExprDsl

trait CirceBuildExprDsl[OP[a] <: HasEncoder[a]] extends BuildExprDsl[OP] with CirceVaporsEncoders
