{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}
module Mesh.Make.Deploy
  (deploymentFiles)
where

import Development.Shake
import Development.Shake.FilePath

import Mesh.Config.Services
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
  { httpbin_service       ∷ FilePath
  , mock_daps_service     ∷ FilePath
  , mongodb_service       ∷ FilePath
  , orion_adapter_service ∷ FilePath
  , orion_service         ∷ FilePath
  }

yamlTargets ∷ BaseDirs → YamlTargets
yamlTargets BaseDirs{..}  = YamlTargets
  { httpbin_service       = deploymentDir </> "httpbin_service.yaml"
  , mock_daps_service     = deploymentDir </> "mock_daps_service.yaml"
  , mongodb_service       = deploymentDir </> "mongodb_service.yaml"
  , orion_adapter_service = deploymentDir </> "orion_adapter_service.yaml"
  , orion_service         = deploymentDir </> "orion_service.yaml"
  }

needHsSourcesIn ∷ FilePath → Action ()
needHsSourcesIn dir = do
  hs ← getDirectoryFiles dir ["//*.hs"]
  need $ fmap (dir </>) hs

writeServiceAndDeployment ∷ FilePath → ServiceSpec → FilePath → Action ()
writeServiceAndDeployment yamsterDir spec = \out → do
  needHsSourcesIn $ yamsterDir </> "src"
  liftIO $ yamlFile out [service spec, deployment spec]

deploymentFiles ∷ FilePath → Rules ()
deploymentFiles repoRoot = do

  let b@BaseDirs{..}  = baseDirs repoRoot
  let YamlTargets{..} = yamlTargets b
  let write = writeServiceAndDeployment yamsterDir

  want [ httpbin_service
       , mock_daps_service
       , mongodb_service
       , orion_adapter_service
       , orion_service
       ]

  httpbin_service       %> write httpbin
  mock_daps_service     %> write mockdaps
  mongodb_service       %> write mongodb
  orion_adapter_service %> write orionadapter
  orion_service         %> write orion
