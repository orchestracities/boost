#!/usr/bin/env bash

GOPATH="$(go env GOPATH)"
# PATH="${GOPATH}/bin:${PATH}"


# see: https://github.com/istio/istio/wiki/Mixer-Out-Of-Process-Adapter-Walkthrough
ISTIO="${GOPATH}/src/istio.io"
ISTIO_REPO="${ISTIO}/istio"
MIXER_REPO="${ISTIO_REPO}/mixer"
