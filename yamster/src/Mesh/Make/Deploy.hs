{-# LANGUAGE UnicodeSyntax #-}
module Mesh.Make.Deploy where

import Mesh.Config.Services
import Mesh.Util.K8s
import Mesh.Util.Yaml

-- TODO delete me!
-- write = yamlFile "t.yaml" [service orionadapter, deployment orionadapter]
-- write = yamlFile "t.yaml" [service httpbin, deployment httpbin]
