{-# LANGUAGE UnicodeSyntax #-}
module Mesh.Config.Routes where

import Mesh.Config.Services
import Mesh.Util.Istio


boostGateway ∷ GatewaySpec
boostGateway = GatewaySpec
  { gatewayName = "boost-gateway"
  , services    = [httpbin]
  }
