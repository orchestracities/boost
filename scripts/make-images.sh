#!/usr/bin/env bash

# Build Docker images for the adapter and the mock DAPS service.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"

source "${ROOTDIR}/scripts/env.sh"

echo "Generating adapter Docker image..."
pushd "${ROOTDIR}" && \
    docker build -t boost/orionadapter -f container/Dockerfile .

echo "Generating mock DAPS Docker image..."
pushd "${ROOTDIR}" && \
    docker build -t boost/mockdaps -f container/mockdaps.dockerfile .
