#!/usr/bin/env bash

# Copy generated adatper cloud infra defs to deployment dir.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
DEPLOYMENTDIR="${ROOTDIR}/deployment"

source "${ROOTDIR}/scripts/env.sh"

echo "Copying orion adapter config..."
cp "${ROOTDIR}/orionadapter/codegen/config/orionadapter.yaml" "${DEPLOYMENTDIR}/"
echo "Copying orion adapter template..."
cp "${ROOTDIR}/orionadapter/codegen/oriondata/template.yaml" "${DEPLOYMENTDIR}/"
