#!/usr/bin/env bash

# Run the mock DAPS server using localhost certs in `deployment/daps`.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
DAPSDIR="${ROOTDIR}/deployment/daps"

source "${ROOTDIR}/scripts/env.sh"

SERVER_KEY="${DAPSDIR}/mockdaps.key-pair.pem"
SERVER_CERT="${DAPSDIR}/mockdaps.localhost.crt.pem"
CLIENT_CERT="${DAPSDIR}/adapter.localhost.crt.pem"

go run "${ROOTDIR}/mockdaps/main.go" 44300 \
    "${SERVER_KEY}" "${SERVER_CERT}" "${CLIENT_CERT}"