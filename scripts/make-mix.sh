#!/usr/bin/env bash

# Build the Mixer server and client from source and copy binaries
# over to out output bin dir.

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
SCRIPTSDIR="${ROOTDIR}/scripts"
OUTBINDIR="${ROOTDIR}/_output_/bin"

source "${ROOTDIR}/scripts/env.sh"

mkdir -p "${OUTBINDIR}"

sh "${SCRIPTSDIR}/get-istio-src.sh"

echo "Buiding Mixer server..."
pushd "${ISTIO_REPO}" && make mixs

echo "Buiding Mixer client..."
pushd "${ISTIO_REPO}" && make mixc


# Figure out target architecture for the build.

LOCAL_ARCH="$(uname -m)"

if [ "${LOCAL_ARCH}" = x86_64 ]
then
    TARGET_ARCH=amd64
elif [ "$(echo ${LOCAL_ARCH} | head -c 5)" = armv8 ]
then
    TARGET_ARCH=arm64
elif [ "$(echo ${LOCAL_ARCH} | head -c 4)" = armv ]
then
    TARGET_ARCH=arm
else
    echo "Unsupported system architecture: ${LOCAL_ARCH}"
    exit 1
fi

# Figure out target OS.

LOCAL_OS="$(uname)"

if [ "${LOCAL_OS}" = Linux ]
then
    TARGET_OS=linux
elif [ "${LOCAL_OS}" = Darwin ]
then
    TARGET_OS=darwin
else
    echo "Unsupported OS: ${LOCAL_OS}"
    exit 1
fi

# Where Go built the binaries when called by the Istio makefile.
TARGET_OUT="${GOPATH}/out/${TARGET_OS}_${TARGET_ARCH}/release"

echo "Copying mixer client & server binaries to: ${OUTBINDIR}/"
cp "${TARGET_OUT}/mixs" "${OUTBINDIR}/"
cp "${TARGET_OUT}/mixc" "${OUTBINDIR}/"
