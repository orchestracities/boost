{-# LANGUAGE UnicodeSyntax, BlockArguments, RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Mesh.Config.Adapter
  ( idsSecHeaderName
  , orionAdapterSpec
  )
where

import Prelude.Unicode
import Data.String.Interpolate (i)
import Peml

import Mesh.Config.AdapterPki
import Mesh.Config.Services (orionadapter, orionadapterGrpcEndpoint,
                             mockdaps, mockdapsHttpsEndpoint)
import Mesh.Util.Istio
import Mesh.Util.K8s (ServiceSpec(..), Port(..), chooseServiceName, serviceFqn)



idsSecHeaderName ∷ String
idsSecHeaderName = "header"

idsAuthzTokenHeaderName ∷ String
idsAuthzTokenHeaderName = "X-AUTH-TOKEN"

fiwareServiceHeaderName ∷ String
fiwareServiceHeaderName = "fiware-service"


-- collect data to generate orionadapter Istio resources.
data OrionAdapterSpec = OrionAdapterSpec
  { service ∷ ServiceSpec
  , pki     ∷ OrionAdapterPki
  }

-- smart constructor.
orionAdapterSpec ∷ OrionAdapterPki → OrionAdapterSpec
orionAdapterSpec pki = OrionAdapterSpec { service = orionadapter
                                        , pki     = pki
                                        }


idTokenJsonTemplate ∷ String
idTokenJsonTemplate = [i|
   {
     "@type": "ids:ResultMessage",
     "id": "http://industrialdataspace.org/resultMessage/%s",
     "issued": "%s",
     "modelVersion": "2.1.0",
     "issuerConnector": "https://companyA.com/connector/59a68243-dd96-4c8d-88a9-0f0e03e13b1b",
     "securityToken": {
       "@type": "ids:DynamicAttributeToken",
       "tokenFormat": "https://w3id.org/idsa/code/tokenformat/JWT",
       "tokenValue": "%s"
     }
   }
|]


-- fill in template for generating orionadapter Istio resources.
instance AdapterSpec OrionAdapterSpec where

  adapterName = serviceName ∘ service

  connectionAddress = address ∘ service
    where
      address spec = inetAddress (chooseServiceName spec)
                                 (servicePort orionadapterGrpcEndpoint)

  handlerParams OrionAdapterSpec{..} = do
    "idsa_public_key" =: idsaPublicKey pki
    "idsa_private_key" =: idsaPrivateKey pki
    "id_token_json_template" =: idTokenJsonTemplate
    "daps" =: do
      "server_host" =: inetAddress (serviceFqn mockdaps)
                                   (servicePort mockdapsHttpsEndpoint)
      "connector_id" =: "4e16f007-d959-4eb2-b47d-78dd0c4eab0e"
      "connector_audience" =: "https://consumerconnector.fiware.org"
      "seconds_before_expiry" =: Z 3600
      "private_key" =: connectorPrivateKey pki
      "connector_certificate" =: connectorCertificate pki
      "server_certificate" =: dapsServerCertificate pki
    "authz" =: do
      "enabled" =: False
      "server_url" =: "http://authzforceingress.appstorecontainerns.46.17.108.63.xip.io/"
      "hs256_shared_secret" =: "d3eafd0101866b21"
      "cache_decision_max_seconds" =: Z 3600

  templateName = const "oriondata"

  instanceParams _ = do
    "request_method" =: "request.method"
    "request_path" =: "request.path"
    "fiware_service" =: header fiwareServiceHeaderName
    "ids_consumer_header" =: header idsSecHeaderName
    "ids_authz_token" =: header idsAuthzTokenHeaderName
    where
      header name = "request.headers[\"" ++ name ++ "\"] | \"\""

  handlerResponseName = const "adapter_response"

  responseHeaderOperations spec = do
    (-:) do
      "name" =: idsSecHeaderName
      "values" =: do
        (-:) $ handlerResponseName spec ++ ".output.context_broker_token"


-- format inet address as "host:port".
inetAddress ∷ String → Integer → String
inetAddress host port = host ++ ":" ++ (show port)
