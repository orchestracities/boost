{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}
module Mesh.Config.Envoy
  (orionEgressFilter)
where

import Data.String.Interpolate (i)
import Peml

import Mesh.Config.Adapter
import Mesh.Config.Services (orionadapter, orionadapterHttpEndpoint, httpbin)
import Mesh.Util.K8s (ServiceSpec(..), Port(..), serviceFqn)


clusterName ∷ String
clusterName = "lua_cluster"

luaFilter ∷ String
luaFilter = [i|
    function envoy_on_request(request_handle)
        local headers, body = request_handle:httpCall(
            "#{clusterName}",
            { [":method"] = "GET",
              [":path"] = "/",
              [":authority"] = "#{clusterName}"
            },
            "",
            5000)
        request_handle:headers():add("#{idsSecHeaderName}", body)
    end
|]

adapterHttpEndpoint ∷ ExprBuilder
adapterHttpEndpoint = do
  "protocol"   =: "TCP"
  "address"    =: serviceFqn orionadapter
  "port_value" =: servicePort orionadapterHttpEndpoint


orionEgressFilter ∷ ExprBuilder
orionEgressFilter = do
  "apiVersion" =: "networking.istio.io/v1alpha3"
  "kind" =: "EnvoyFilter"
  "metadata" =: do
    "name" =: "orion-egress-filter"
    "namespace" =: "default"
  "spec" =: do
    "workloadSelector" =: do          -- TODO how to select more than one?
      "labels" =: do                  -- E.g. what if we want to have both
        "app" =: serviceName httpbin  -- httpbin and orion? Go type is map...
    "configPatches" =: do
    -- This patch adds the lua filter to the listener/http connection manager
      (-:) $ do
        "applyTo" =: "HTTP_FILTER"
        "match" =: do
          "context" =: "SIDECAR_OUTBOUND"
          "listener" =: do
            "portNumber" =: Z 80
            "filterChain" =: do
              "filter" =: do
                "name" =: "envoy.http_connection_manager"
                "subFilter" =: do
                  "name" =: "envoy.router"
        "patch" =: do
          "operation" =: "INSERT_BEFORE"
          "value" =: do  -- lua filter specification
            "name" =: "envoy.lua"
            "config" =: do
              "inlineCode" =: luaFilter
    -- The second patch adds the cluster that is referenced by the lua code
    -- cds match is omitted as a new cluster is being added
      (-:) $ do
        "applyTo" =: "CLUSTER"
        -- "match" =: do
        --   "context" =: "SIDECAR_OUTBOUND"
        "patch" =: do
          "operation" =: "ADD"
          "value" =: do  -- cluster specification
            "name" =: clusterName
            "type" =: "STRICT_DNS"
            "connect_timeout" =: "5.5s"
            "lb_policy" =: "ROUND_ROBIN"
            "hosts" =: do
              (-:) $ do
                "socket_address" =: adapterHttpEndpoint
                "socket_address" =: adapterHttpEndpoint  -- (*)
--
-- (*) NOTE. PEML list of tags.
-- In the Builder monad, a list of tags gets converted to a map, but here
-- we have to generate a list of one tag! e.g.
--
--      hosts:
--      - socket_address:
--          protocol: TCP
--          address: "orionadapterservice.istio-system"
--          port_value: 54321
--
-- To get around this, we add socket_address twice so that instead of
-- a one-element list with a tag (= map with one key/val) we build a
-- one-element list with a map. In the end duplicates get ditched
-- when converting to YAML anyway.
--
