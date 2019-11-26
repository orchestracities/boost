#!/usr/bin/env bash

# Clone the Istio repo in the GOPATH where Istio codegen expects it to be.
# If cloned already, clean it up since an earlier run to generate our adpater
# config would leave around quite a bit of junk.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"

source "${ROOTDIR}/scripts/env.sh"

echo "Generating adapter Docker image..."
pushd "${ROOTDIR}" && \
    docker build -t boost/orionadapter -f container/Dockerfile .
