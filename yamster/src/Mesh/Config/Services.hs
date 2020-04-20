{-# LANGUAGE UnicodeSyntax, BlockArguments, RecordWildCards #-}
module Mesh.Config.Services where

import Data.Default

import Mesh.Util.Istio
import Mesh.Util.K8s


httpbin ∷ ServiceSpec
httpbin = def
  { serviceName = "httpbin"
  , version     = Just "v1"
  , image       = "docker.io/kennethreitz/httpbin"
  , ports       =
    [ http { servicePort   = 8000
           , containerPort = Just 80
           , externalPort  = Just 80
           }
    ]
  }

orionadapterHttpEndpoint ∷ Port
orionadapterHttpEndpoint = http { servicePort = 54321 }

orionadapterGrpcEndpoint ∷ Port
orionadapterGrpcEndpoint = grpc { servicePort = 43210 }

orionadapter ∷ ServiceSpec
orionadapter = def
  { serviceName     = "orionadapter"
  , metaName        = Just "orionadapterservice"  -- see TODO in K8s module!
  , namespace       = Just istioNamespace
  , image           = "boost/orionadapter:latest"
  , imagePullPolicy = Never
  , serviceType     = ClusterIP
  , ports           = [ orionadapterGrpcEndpoint, orionadapterHttpEndpoint ]
  , withSideCar = False
  }

mockdapsHttpsEndpoint ∷ Port
mockdapsHttpsEndpoint = https { servicePort = 44300 }

mockdaps ∷ ServiceSpec
mockdaps = def
  { serviceName     = "mockdaps"
  , image           = "boost/mockdaps:latest"
  , imagePullPolicy = Never
  , serviceType     = ClusterIP
  , ports           = [ mockdapsHttpsEndpoint ]
  , withSideCar = False
  }

-- WARNING. only use for testing as DB data isn't persisted!
mongodb ∷ ServiceSpec
mongodb = def
  { serviceName = "mongodb"
  , image       = "mongo:3.6"
  , command     = CmdStr "mongod --bind_ip_all --smallfiles"
  , ports       = [ mongo { servicePort = 27017 } ]
  , withSideCar = False
  }


startOrion ∷ ContainerCommand
startOrion = Cmd "bash" ["-c", script]
  where
    script =  "sleep 4; "  -- (*)
           ++ "exec /usr/bin/contextBroker -fg -multiservice -ngsiv1Autocast "
           ++ "-dbhost " ++ serviceName mongodb
           ++ " -logLevel DEBUG"
-- NOTE
-- (*) Poor man's approach to start-up race conditions and service
-- dependencies.
-- See: https://github.com/orchestracities/boost/issues/28
--

orion ∷ ServiceSpec
orion = def
  { serviceName     = "orion"
  , image           = "fiware/orion:2.2.0"
  , command         = startOrion
  , ports           =
    [ http                         -- (1)
      { servicePort   = 1026
      , externalPort  = Just 1026  -- (2)
      }
    ]
  }
--
-- NOTE
-- (1) Port name. Surely `ngsi` would make more sense here, but if we
-- set it to anything else than `http`, then the sidecar stops forwarding
-- messages to Orion. Uh, WTH.
-- See: https://github.com/orchestracities/boost/issues/28
--
-- (2) Custom external ports.
-- The Istio demo profile exposes some common ports like 80 and 443
-- through load-balancer cluster ports 30072 and 31515, respectively.
-- We have to add our Orion port manually to the load balancer config:
--
--     $ EDITOR=emacs kubectl -n istio-system edit svc istio-ingressgateway
--
-- then add the below port to the ports section:
--
--     ports:
--     ...
--       - name: orion
--         nodePort: 31026
--         port: 1026
--         protocol: TCP
--         targetPort: 1026
--
-- See: https://stackoverflow.com/questions/56661765/
--
