#!/usr/bin/env bash

# Use the Mixer client to send an IDS-DTH token to a local Mixer server.
# Pass the JWT token as the first argument to this script or call the
# script with no arguments to send an empty token.

set -e

TOKENARG=$1

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
OUTBINDIR="${ROOTDIR}/_output_/bin"
TESTDATADIR="${ROOTDIR}/_output_/testdata"

source "${ROOTDIR}/scripts/env.sh"

"${OUTBINDIR}/mixc" check -s destination.service="svc.cluster.local" \
    --stringmap_attributes "request.headers=ids-dth:${TOKENARG}"
