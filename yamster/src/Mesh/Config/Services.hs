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
    [ def { portName      = Just "http"
          , servicePort   = 8000
          , containerPort = Just 80
          , externalPort  = Just 80
          }
    ]
  }

orionadapter ∷ ServiceSpec
orionadapter = def
  { serviceName     = "orionadapter"
  , metaName        = Just "orionadapterservice"  -- see TODO in K8s module!
  , namespace       = Just istioNamespace
  , image           = "boost/orionadapter:latest"
  , imagePullPolicy = Never
  , serviceType     = ClusterIP
  , ports           =
    [ def { portName      = Just "grpc"
          , servicePort   = 43210
          }
    , def { portName      = Just "http"
          , servicePort   = 54321
          }
    ]
  , withSideCar = False
  }

mockdaps ∷ ServiceSpec
mockdaps = def
  { serviceName     = "mockdaps"
  , image           = "boost/mockdaps:latest"
  , imagePullPolicy = Never
  , serviceType     = ClusterIP
  , ports           =
    [ def { portName      = Just "https"
          , servicePort   = 44300
          }
    ]
  , withSideCar = False
  }

-- WARNING. only use for testing as DB data isn't persisted!
mongodb ∷ ServiceSpec
mongodb = def
  { serviceName     = "mongodb"
  , image           = "mongo:3.6"
  , command         = Just "mongod --bind_ip_all --smallfiles"
  , ports           =
    [ def { portName      = Just "mongo"
          , servicePort   = 27017
          }
    ]
  , withSideCar = False
  }

orion ∷ ServiceSpec
orion = def
  { serviceName     = "orion"
  , image           = "fiware/orion:2.2.0"
  , command         = Just $
                      "/usr/bin/contextBroker -fg -multiservice " ++
                      "-ngsiv1Autocast -dbhost " ++ serviceName mongodb ++
                      " -logLevel DEBUG"
  , ports           =
    [ def { portName      = Just "ngsi"
          , servicePort   = 1026
          }
    ]
  }
