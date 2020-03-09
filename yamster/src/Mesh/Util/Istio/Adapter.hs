{-# LANGUAGE UnicodeSyntax, BlockArguments #-}
module Mesh.Util.Istio.Adapter
  ( AdapterSpec(..)
  , handler
  , instens
  , rule
  )
where

import Prelude.Unicode
import Peml

import Mesh.Util.Istio.Namespace (istioNamespace)


-- adapter template to generate handler/instance/rule config.
class AdapterSpec ξ where

  adapterName ∷ ξ → String

  handlerName ∷ ξ → String
  handlerName = (++ "-handler") ∘ adapterName

  connectionAddress ∷ ξ → String
  handlerParams ∷ ξ → ExprBuilder

  instanceName ∷ ξ → String
  instanceName = (++ "-instance") ∘ adapterName

  templateName ∷ ξ → String
  instanceParams ∷ ξ → ExprBuilder

  ruleName ∷ ξ → String
  ruleName = (++ "-rule") ∘ adapterName

  handlerResponseName ∷ ξ → String
  responseHeaderOperations ∷ ξ → ExprBuilder


fqHandlerName ∷ AdapterSpec ξ ⇒ ξ → String
fqHandlerName = (++ istioNamespace) ∘ (++ ".") ∘ handlerName

spec ∷ String → String → ExprBuilder → ExprBuilder
spec kind name expr = do
  "apiVersion" =: "config.istio.io/v1alpha2"
  "kind" =: kind
  "metadata" =: do
    "name" =: name
    "namespace" =: istioNamespace
  "spec" =: expr

handler ∷ AdapterSpec ξ ⇒ ξ → ExprBuilder
handler s = spec "handler" (handlerName s) $ do
  "adapter" =: adapterName s
  "connection" =: do
    "address" =: connectionAddress s
  "params" =: handlerParams s

instens ∷ AdapterSpec ξ ⇒ ξ → ExprBuilder  -- instance is a reserved word
instens s = spec "instance" (instanceName s) $ do
  "template" =: templateName s
  "params" =: instanceParams s

rule ∷ AdapterSpec ξ ⇒ ξ → ExprBuilder
rule s = spec "rule" (ruleName s) $ do
  "actions" =: do
    (-:) $ do
      "handler" =: fqHandlerName s
      "instances" =: do
        (-:) $ instanceName s
      "name" =: handlerResponseName s
  "responseHeaderOperations" =: responseHeaderOperations s
