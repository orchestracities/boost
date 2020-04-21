{-# LANGUAGE UnicodeSyntax #-}
module Mesh.Util.Istio.Ports
where

import Data.Default (def)

import Mesh.Util.K8s (Port (..))


-- Istio uses K8s port names to select protocols! We capture that here
-- to avoid costly config mistakes.
-- See:
-- https://archive.istio.io/v1.4/docs/ops/configuration/traffic-management/protocol-selection/
-- https://github.com/orchestracities/boost/issues/28

grpc ∷ Port
grpc = def { portName = Just "grpc" }

grcpweb ∷ Port
grcpweb = def { portName = Just "grpc-web" }

http ∷ Port
http = def { portName = Just "http" }

http2 ∷ Port
http2 = def { portName = Just "http2" }

https ∷ Port
https = def { portName = Just "https" }

mongo ∷ Port
mongo = def { portName = Just "mongo" }

mysql ∷ Port
mysql = def { portName = Just "mysql" }

redis ∷ Port
redis = def { portName = Just "redis" }

tcp ∷ Port
tcp = def { portName = Just "tcp" }

tls ∷ Port
tls = def { portName = Just "tls" }

udp ∷ Port
udp = def { portName = Just "udp" }
