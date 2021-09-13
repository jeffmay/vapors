package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.VaporsDsl

trait CirceVaporsDsl[OP[a] <: HasEncoder[a]] extends VaporsDsl[OP] with VaporsCirceEncoders
