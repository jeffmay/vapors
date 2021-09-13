package com.rallyhealth

package vapors.v1.dsl.circe

import vapors.v1.dsl.VaporsDsl

trait CirceVaporsDsl[OP[a] <: HasEncoder[a]] extends VaporsDsl[OP] with VaporsCirceEncoders
