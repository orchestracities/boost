#!/usr/bin/env bash

# Run the Mixer server using our testdata dir as config store.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
TESTDATADIR="${ROOTDIR}/testdata"

source "${ROOTDIR}/scripts/env.sh"

BIN="${GOPATH}/out/darwin_amd64/release"
# TODO path will be different on other OSes, figure out a better way of doing
# this!!
"${BIN}/mixs" server --configStoreURL="fs://${TESTDATADIR}"
