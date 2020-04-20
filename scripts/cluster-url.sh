#!/usr/bin/env bash

set -e

export INGRESS_HOST=$(minikube ip)
export INGRESS_PORT=$(kubectl -n istio-system \
                      get service istio-ingressgateway \
                      -o jsonpath='{.spec.ports[?(@.name=="http2")].nodePort}')

export BASE_URL="http://${INGRESS_HOST}:${INGRESS_PORT}"

export ORION_INGRESS_PORT=$(kubectl \
    -n istio-system get service istio-ingressgateway \
    -o jsonpath='{.spec.ports[?(@.name=="orion")].nodePort}')
export ORION_BASE_URL="http://${INGRESS_HOST}:${ORION_INGRESS_PORT}"
