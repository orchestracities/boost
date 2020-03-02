{-# LANGUAGE UnicodeSyntax, BlockArguments, RecordWildCards #-}
module Mesh.Util.Istio.Ingress
  ( GatewaySpec(..)
  , routeIngressHttp
  )
where

import Prelude.Unicode
import Control.Arrow ((&&&))
import Data.Maybe (isJust, fromJust)
import Peml

import Mesh.Util.K8s (ServiceSpec (..), Port (..))


-- | Input spec to generate Istio gateway and virtual services.
data GatewaySpec = GatewaySpec
  { gatewayName  ∷ String
  , services     ∷ [ServiceSpec]
  }
  deriving Show


-- assembly of handy data derived from input spec.

data PortRoute = PortRoute
  { gatewayPort ∷ Integer
  , targetPort  ∷ Integer
  }
  deriving Show

portRoutes ∷ ServiceSpec → [PortRoute]
portRoutes = fmap toPortRoute ∘ filter hasExtPort ∘ ports
  where
    hasExtPort    = isJust ∘ externalPort
    toPortRoute p = PortRoute { gatewayPort = fromJust $ externalPort p
                              , targetPort  = servicePort p
                              }

servicePortRoutes ∷ ServiceSpec → [(ServiceSpec, PortRoute)]
servicePortRoutes = uncurry zip ∘ (repeat &&& portRoutes)

hasPortRoutes ∷ ServiceSpec → Bool
hasPortRoutes = (> 0) ∘ length ∘ servicePortRoutes

serviceWithPortRoutes ∷ GatewaySpec → [ServiceSpec]
serviceWithPortRoutes = filter hasPortRoutes ∘ services

portRoutesByService ∷ GatewaySpec → [(ServiceSpec, PortRoute)]
portRoutesByService = concatMap servicePortRoutes ∘ services


-- Istio gateway resource.

gateway ∷ GatewaySpec → ExprBuilder
gateway gw@GatewaySpec{..} = do
  "apiVersion" =: "networking.istio.io/v1alpha3"
  "kind" =: "Gateway"
  "metadata" =: do
    "name" =: gatewayName
  "spec" =: do
    "selector" =: do
      "istio" =: "ingressgateway" -- TODO is it enabled in every Istio profile?
    "servers" =: listServers gw

listServers ∷ GatewaySpec → ExprBuilder
listServers = mapM_ (-:) ∘ fmap server ∘ portRoutesByService

server ∷ (ServiceSpec, PortRoute) → ExprBuilder
server (ServiceSpec{..}, PortRoute{..}) = do
  "port"  =: do
    "number"   =: gatewayPort
    "name"     =: name
    "protocol" =: "HTTP"
  "hosts" =: do
    (-:) "*"
  where
    name = serviceName ++ ":" ++ (show gatewayPort)


-- Istio virtual service resource

virtualService ∷ String → ServiceSpec → ExprBuilder
virtualService gatewayName spec@ServiceSpec{..} = do
  "apiVersion" =: "networking.istio.io/v1alpha3"
  "kind" =: "VirtualService"
  "metadata" =: do
    "name" =: serviceName
  "spec" =: do
    "hosts" =: do
      (-:) "*"
    "gateways" =: do
      (-:) gatewayName
    "http" =: listRoutes spec

listRoutes ∷ ServiceSpec → ExprBuilder
listRoutes = mapM_ (-:) ∘ fmap route ∘ servicePortRoutes

route ∷ (ServiceSpec, PortRoute) → ExprBuilder
route (ServiceSpec{..}, PortRoute{..}) = do
  "match" =: do
    (-:) $ do
      "port" =: gatewayPort
      "port" =: gatewayPort  -- (*)
  "route" =: do
    (-:) $ do
      "destination" =: do
        "host" =: serviceName
        "port" =: do
          "number" =: targetPort
      "weight" =: Z 100
--
-- (*) NOTE. PEML list of tags.
-- In the Builder monad, a list of tags gets converted to a map, but here
-- we have to generate a list of one tag! e.g.
--
--     match:
--     - port: 80
--     route:
--       ...
--
-- To get around this, we add the port twice so that instead of a
-- one-element list with a tag (= map with one key/val) we build a
-- one-element list with a map. In the end duplicates get ditched
-- when converting to YAML anyway.
--


-- | Simple port-based routing of incoming HTTP service requests.
-- Build an Istio gateway resource exposing each external port found
-- in the input services list. Then build an Istio virtual service
-- resource in correspondence of each input service that has external
-- ports—skip services with no external ports. The virtual service
-- resource maps each service's external port added to the gateway to
-- the corresponding service port in the mesh.
--
-- For example, say you have a gateway spec with three services
--
-- @
--    gw = GatewaySpec
--       { gatewayName = "my-gateway"
--       , services    = [s1, s2, s3]
--       }
-- @
--
-- and ports
--
-- @
--     s1:
--       - service port: 8000
--         external port:  80
--       - service port: 8001
--         external port:   ∅  -- no external port, this is an internal port
--       - service port:  443
--       - external port: 443
--
--     s2: no external ports
--
--     s3:
--       - service port: 7000
--         external port:  70
-- @
--
-- Then ports @80@, @443@ and @70@ get exposed by the gateway and there
-- will be two virtual services, one for @s1@ and one for @s3@—@s2@
-- gets ignored since it has no external ports. Virtual service @s1@
-- routes HTTP requests hitting the gateway on port @80@ and @443@,
-- respectively, to @s1@ mesh service port @8000@ and @443@. Likewise,
-- virtual service @s3@ routes incoming traffic from port @70@ to port
-- @7000@.
--
routeIngressHttp ∷ GatewaySpec → [ExprBuilder]
routeIngressHttp gw = gateway gw : virtualServices
  where
    virtualServices  = fmap toVirtualService $ serviceWithPortRoutes gw
    toVirtualService = virtualService (gatewayName gw)
