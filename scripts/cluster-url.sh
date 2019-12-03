#!/usr/bin/env bash

export INGRESS_PORT=$(kubectl -n istio-system get service istio-ingressgateway -o jsonpath='{.spec.ports[?(@.name=="http2")].nodePort}')
export INGRESS_HOST=$(minikube ip)
export BASE_URL="http://${INGRESS_HOST}:${INGRESS_PORT}"
