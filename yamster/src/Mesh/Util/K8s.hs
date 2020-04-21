{-# LANGUAGE UnicodeSyntax, BlockArguments, RecordWildCards #-}
module Mesh.Util.K8s
  ( ImagePullPolicy (..)
  , ServiceType (..)
  , Protocol (..)
  , Port (..)
  , ContainerCommand (..)
  , ServiceSpec (..)
  , serviceFqn
  , chooseServiceName
  , service
  , deployment
  )
where

import Prelude.Unicode
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Peml



data ImagePullPolicy = IfNotPresent | Never
  deriving Show

data ServiceType = ClusterIP | NodePort
  deriving Show

data Protocol = TCP | UDP
  deriving Show

data Port = Port
  { portName      ∷ Maybe String  -- default: don't add
  , protocol      ∷ Protocol -- default: TCP
  , servicePort   ∷ Integer
  , containerPort ∷ Maybe Integer -- default: servicePort
  , externalPort  ∷ Maybe Integer -- expose service if present,
                                  -- see Istio/Ingress.hs
  }
  deriving Show

chooseContainerPort ∷ Port → Integer
chooseContainerPort (Port{..}) = fromMaybe servicePort containerPort

instance Default Port where
  def = Port { portName      = def
             , protocol      = TCP
             , servicePort   = def
             , containerPort = def
             , externalPort  = def
             }

instance ToPeml Port where
  toPeml (p @ Port{..})= build $ do
    maybe skip ("name" =:) portName
    "protocol"         =:  show protocol
    "port"             =:  servicePort
    "targetPort"       =:  chooseContainerPort p


data ContainerCommand = Cmd { program ∷ String, args ∷ [String] }
                      | CmdStr String
                      | ContainerDefault
                      deriving Show
-- Cmd "mongodb" ["--bind_ip_all", "--smallfiles"]
-- Cmd "/bin/bash" ["-c", "sleep 4 && start-pgm"]
-- CmdStr "mongod --bind_ip_all --smallfiles"

instance Default ContainerCommand where
  def = ContainerDefault

tagList ∷ ToPeml ξ ⇒ String → [ξ] → ExprBuilder
tagList name = (name =:) ∘ mapM_ (-:)

tagCommand ∷ String → ContainerCommand → ExprBuilder
tagCommand _ ContainerDefault = skip
tagCommand name Cmd{..}       = tagList name $ program:args
tagCommand name (CmdStr cmd)  = tagList name ∘ words $ cmd  -- (*)
-- (*) TODO CmdStr parsing.
-- Ideally we should handle quotes so that e.g.
--     "bash -c \"sleep 4 && start-pgm\""
-- gets parsed to
--     ["bash", "-c", "sleep 4 && start-pgm"]
-- instead of (what words does)
--     ["bash", "-c", "\"sleep", "4", "&&", "start-pgm\""]


data ServiceSpec = ServiceSpec
  { serviceName     ∷ String
  , metaName        ∷ Maybe String -- default: serviceName    (*)
  , namespace       ∷ Maybe String
  , version         ∷ Maybe String
  , serviceType     ∷ ServiceType -- default: NodePort
  , replicas        ∷ Integer -- default: 1
  , ports           ∷ [Port]
  , image           ∷ String
  , imagePullPolicy ∷ ImagePullPolicy -- default: IfNotPresent
  , command         ∷ ContainerCommand
  , withSideCar     ∷ Bool -- default: True, i.e. let Istio add sidecar
  }
  deriving Show
-- (*) TODO ditch metaName field.
-- At the moment, we only need it for orionserviceadapter since it's
-- meta name is different than the service name: orionservice. I didn't
-- go ahead with renaming since there's quite a number of things to do,
-- as you can see from grepping in the repo root dir:
--
--     $ grep -R orionadapterservice .
--

instance Default ServiceSpec where
  def = ServiceSpec { serviceName     = def
                    , metaName        = def
                    , namespace       = def
                    , version         = def
                    , serviceType     = NodePort
                    , replicas        = 1
                    , ports           = def
                    , image           = def
                    , imagePullPolicy = IfNotPresent
                    , command         = def
                    , withSideCar     = True
                    }

serviceFqn ∷ ServiceSpec → String
serviceFqn s@ServiceSpec{..} = chooseServiceName s ++ "."
                             ++ fromMaybe "default" namespace

chooseServiceName ∷ ServiceSpec → String
chooseServiceName ServiceSpec{..} = fromMaybe serviceName metaName

service ∷ ServiceSpec → ExprBuilder
service ServiceSpec{..} = do
  "apiVersion" =: "v1"
  "kind" =: "Service"
  "metadata" =: do
    "name" =: fromMaybe serviceName metaName
    maybe skip ("namespace" =:) namespace
    "labels" =: do
      "app" =: serviceName
  "spec" =: do
    "type" =: show serviceType
    "ports" =: do
      mapM_ (-:) ports
    "selector" =: do
      "app" =: serviceName


container ∷ ServiceSpec → ExprBuilder
container ServiceSpec{..} = do
  "name" =: serviceName
  "image" =: image
  "imagePullPolicy" =: show imagePullPolicy
  tagCommand "command" command
  "ports" =: do
    mapM_ (-:) namedPorts
  where
    namedPorts = fmap namePort ports  -- (*)
    namePort (p @ Port{..}) = do
      "name" =: fromMaybe "" portName
      "containerPort" =: chooseContainerPort p
--
-- (*) NOTE. PEML list of tags.
-- In the Builder monad, a list of tags gets converted to a map, but here
-- we have to generate a list of tags! e.g.
--
--     ports:
--     - containerPort: 11000
--     - containerPort: 22000
--
-- To get around this, we add the port name too even if it's missing
-- from the input ServiceSpec.
--

sidecar ∷ ServiceSpec → ExprBuilder
sidecar ServiceSpec{..}
  | withSideCar = skip
  | otherwise = do
      "annotations" =: do
        "sidecar.istio.io/inject" =: "false"
        "scheduler.alpha.kubernetes.io/critical-pod" =: ""

deployment ∷ ServiceSpec → ExprBuilder
deployment spec @ ServiceSpec{..} = do
  "apiVersion" =: "apps/v1"
  "kind" =: "Deployment"
  "metadata" =: do
    "name" =: serviceName
    maybe skip ("namespace" =:) namespace
    "labels" =: do
      "app" =: serviceName
  "spec" =: do
    "replicas" =: replicas
    "selector" =: do
      "matchLabels" =: do
        "app" =: serviceName
        maybe skip ("version" =:) version
    "template" =: do
      "metadata" =: do
        "labels" =: do
          "app" =: serviceName
          maybe skip ("version" =:) version
        sidecar spec
      "spec" =: do
        "containers" =: do
          (-:) (container spec)
