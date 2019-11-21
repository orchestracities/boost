#!/usr/bin/env bash

# Build the Mixer server and client from source.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
SCRIPTSDIR="${ROOTDIR}/scripts"

source "${SCRIPTSDIR}/env.sh"

sh "${SCRIPTSDIR}/get-istio-src.sh"

echo "Buiding Mixer server..."
pushd "${ISTIO_REPO}" && make mixs

echo "Buiding Mixer client..."
pushd "${ISTIO_REPO}" && make mixc
