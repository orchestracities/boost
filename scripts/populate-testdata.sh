#!/usr/bin/env bash

# Copy YAML config to testdata dir so that the Mixer server can use it as
# its config dir.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
TESTDATADIR="${ROOTDIR}/_output_/testdata"
DAPSDIR="${ROOTDIR}/deployment/daps"

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
        -e 's/mockdaps.default:44300/localhost:44300/g' > \
    "${TESTDATADIR}/sample_operator_cfg.yaml"

echo "Generating DAPS config for localhost..."
CONNECTOR_CERT=$(sed  's/^/      /' "${DAPSDIR}/adapter.localhost.crt.pem")
MOCKDAPS_CERT=$(sed  's/^/      /' "${DAPSDIR}/mockdaps.localhost.crt.pem")

echo "    connector_certificate: |" > "${TESTDATADIR}/adapter.daps.certs"
echo "${CONNECTOR_CERT}" >> "${TESTDATADIR}/adapter.daps.certs"
echo "    server_certificate: |" >> "${TESTDATADIR}/adapter.daps.certs"
echo "${MOCKDAPS_CERT}" >> "${TESTDATADIR}/adapter.daps.certs"

echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
echo "\t !!ADDTIONAL MANUAL STEP REQUIRED!!"
echo "\t Replace the content of connector_certificate and server_certificate"
echo "\t in ${TESTDATADIR}/sample_operator_cfg.yaml"
echo "\t with that of ${TESTDATADIR}/adapter.daps.certs"
echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
