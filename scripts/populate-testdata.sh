#!/usr/bin/env bash

# Copy YAML config to testdata dir so that the Mixer server can use it as
# its config dir.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
TESTDATADIR="${ROOTDIR}/testdata"

source "${ROOTDIR}/scripts/env.sh"

echo "Copying orion adapter config..."
cp "${ROOTDIR}/orionadapter/config/orionadapter.yaml" "${TESTDATADIR}/"

echo "Copying mixer config..."
cp "${MIXER_REPO}/testdata/config/attributes.yaml" "${TESTDATADIR}/"
cp "${MIXER_REPO}/template/authorization/template.yaml" "${TESTDATADIR}/"
