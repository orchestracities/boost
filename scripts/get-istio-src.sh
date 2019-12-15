#!/usr/bin/env bash

# Clone the Istio repo in the GOPATH where Istio codegen expects it to be.
# If cloned already, clean it up since an earlier run to generate our adpater
# config would leave around quite a bit of junk.
#
# TODO: pin repo version to revision of `1.4.2` tag.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"

source "${ROOTDIR}/scripts/env.sh"

echo "getting Istio sources into: ${ISTIO_REPO}"
if [ -d "${ISTIO_REPO}" ] ; then
    echo "${ISTIO_REPO} already exists, cleaning up any dirt from previous runs"
    cd "${ISTIO_REPO}"
    git checkout -- .
    git clean -f -d .
else
    mkdir -p "${ISTIO_REPO}"
    git clone https://github.com/istio/istio "${ISTIO_REPO}"
fi
