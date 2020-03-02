{-# LANGUAGE UnicodeSyntax #-}
module Mesh.Util.Istio
  ( istioNamespace
  , module Mesh.Util.Istio.Ingress
  )
where

import Mesh.Util.Istio.Ingress


istioNamespace ∷ String
istioNamespace = "istio-system"

