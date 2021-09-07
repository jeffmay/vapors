package com.rallyhealth

package vapors.v1.dsl

import io.circe.Encoder

package object circe extends VaporsDsl[Encoder] with VaporsCirceEncoders
