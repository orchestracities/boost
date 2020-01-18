#!/usr/bin/env bash

# Copy YAML config to testdata dir so that the Mixer server can use it as
# its config dir.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
TESTDATADIR="${ROOTDIR}/_output_/testdata"

source "${ROOTDIR}/scripts/env.sh"

mkdir -p "${TESTDATADIR}"

echo "Copying orion adapter config..."
cp "${ROOTDIR}/orionadapter/codegen/config/orionadapter.yaml" "${TESTDATADIR}/"
echo "Copying orion adapter template..."
cp "${ROOTDIR}/orionadapter/codegen/oriondata/template.yaml" "${TESTDATADIR}/"

echo "Copying mixer config..."
cp "${MIXER_REPO}/testdata/config/attributes.yaml" "${TESTDATADIR}/"

echo "Generating operator config..."
cat "${ROOTDIR}/deployment/sample_operator_cfg.yaml" | \
    sed -e 's/orionadapterservice:43210/[::]:43210/g' \
        -e 's/mockdaps:44300/localhost:44300/g' > \
    "${TESTDATADIR}/sample_operator_cfg.yaml"
