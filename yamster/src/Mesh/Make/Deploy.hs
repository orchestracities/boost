{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}
module Mesh.Make.Deploy
  (deploymentFiles)
where

import Control.Monad.IO.Class (MonadIO)
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
  , orion_adapter_service ∷ FilePath
  }

yamlTargets ∷ BaseDirs → YamlTargets
yamlTargets BaseDirs{..} = YamlTargets
  { httpbin_service       = deploymentDir </> "httpbin_service.yaml"
  , orion_adapter_service = deploymentDir </> "orion_adapter_service.yaml"
  }

needHsSourcesIn ∷ FilePath → Action ()
needHsSourcesIn dir = do
  hs ← getDirectoryFiles (dir </> "src") ["//*.hs"]
  need $ fmap ((dir </> "src") </>) hs

writeServiceAndDeploymentResources ∷ MonadIO m ⇒ FilePath → ServiceSpec -> m ()
writeServiceAndDeploymentResources file spec =
  liftIO $ yamlFile file [service spec, deployment spec]


deploymentFiles ∷ FilePath → Rules ()
deploymentFiles repoRoot = do

  let b@BaseDirs{..}  = baseDirs repoRoot
  let YamlTargets{..} = yamlTargets b

  want [ httpbin_service
       , orion_adapter_service
       ]

  httpbin_service %> \out → do
    needHsSourcesIn yamsterDir
    writeServiceAndDeploymentResources out httpbin

  orion_adapter_service %> \out → do
    needHsSourcesIn yamsterDir
    writeServiceAndDeploymentResources out orionadapter
