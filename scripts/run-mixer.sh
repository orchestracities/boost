#!/usr/bin/env bash

# Run the Mixer server using our testdata dir as config store.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
OUTBINDIR="${ROOTDIR}/_output_/bin"
TESTDATADIR="${ROOTDIR}/_output_/testdata"

source "${ROOTDIR}/scripts/env.sh"

"${OUTBINDIR}/mixs" server --configStoreURL="fs://${TESTDATADIR}"
