{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}
module Mesh.Make.Deploy
  (deploymentFiles)
where

import Prelude.Unicode
import Development.Shake
import Development.Shake.FilePath
import Peml

import Mesh.Config.Envoy
import Mesh.Config.Routes
import Mesh.Config.Services
import Mesh.Util.Istio
import Mesh.Util.K8s
import Mesh.Util.Yaml


data BaseDirs = BaseDirs
  { rootDir       ∷ FilePath
  , deploymentDir ∷ FilePath
  , yamsterDir    ∷ FilePath
  }

baseDirs ∷ FilePath → BaseDirs
baseDirs repoRoot = BaseDirs
  { rootDir       = repoRoot
  , deploymentDir = repoRoot </> "deployment"
  , yamsterDir    = repoRoot </> "yamster"
  }

data YamlTargets = YamlTargets
  { egress_filter         ∷ FilePath
  , httpbin_service       ∷ FilePath
  , ingress_routing       ∷ FilePath
  , mock_daps_service     ∷ FilePath
  , mongodb_service       ∷ FilePath
  , orion_adapter_service ∷ FilePath
  , orion_service         ∷ FilePath
  }

yamlTargets ∷ BaseDirs → YamlTargets
yamlTargets BaseDirs{..}  = YamlTargets
  { egress_filter         = deploymentDir </> "egress_filter.yaml"
  , httpbin_service       = deploymentDir </> "httpbin_service.yaml"
  , ingress_routing       = deploymentDir </> "ingress_routing.yaml"
  , mock_daps_service     = deploymentDir </> "mock_daps_service.yaml"
  , mongodb_service       = deploymentDir </> "mongodb_service.yaml"
  , orion_adapter_service = deploymentDir </> "orion_adapter_service.yaml"
  , orion_service         = deploymentDir </> "orion_service.yaml"
  }

needHsSourcesIn ∷ FilePath → Action ()
needHsSourcesIn dir = do
  hs ← getDirectoryFiles dir ["//*.hs"]
  need $ fmap (dir </>) hs

writeExprs ∷ FilePath → [ExprBuilder] → FilePath → Action ()
writeExprs yamsterDir xs = \out → do
  needHsSourcesIn $ yamsterDir </> "src"
  liftIO $ yamlFile out xs

writeServiceAndDeployment ∷ FilePath → ServiceSpec → FilePath → Action ()
writeServiceAndDeployment yamsterDir spec =
  writeExprs yamsterDir [service spec, deployment spec]

writeRoutes ∷ FilePath → GatewaySpec → FilePath → Action ()
writeRoutes yamsterDir = writeExprs yamsterDir ∘ routeIngressHttp

deploymentFiles ∷ FilePath → Rules ()
deploymentFiles repoRoot = do

  let b@BaseDirs{..}  = baseDirs repoRoot
  let YamlTargets{..} = yamlTargets b
  let writeS = writeServiceAndDeployment yamsterDir
  let writeR = writeRoutes yamsterDir
  let writeE = writeExprs yamsterDir ∘ pure

  want [ egress_filter
       , httpbin_service
       , ingress_routing
       , mock_daps_service
       , mongodb_service
       , orion_adapter_service
       , orion_service
       ]

  egress_filter         %> writeE orionEgressFilter
  httpbin_service       %> writeS httpbin
  ingress_routing       %> writeR boostGateway
  mock_daps_service     %> writeS mockdaps
  mongodb_service       %> writeS mongodb
  orion_adapter_service %> writeS orionadapter
  orion_service         %> writeS orion
