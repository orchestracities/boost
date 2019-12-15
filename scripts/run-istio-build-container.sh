#!/usr/bin/env bash

# Run the specified command in the Istio build tools Docker image.
# You get dropped in a container where you can build Istio stuff and your
# local Istio repo gets mounted on the container's working dir. (Make sure
# you've run `get-istio-src.sh` before running this script!)
# Examples:
#
#     $ ./istio-build-container.sh sh
#     $ ./istio-build-container.sh make
#     $ ./istio-build-container.sh make go-gen
#
# With this image we can generate Istio code for the adapter without
# having to install locally the Istio build env & deps.
# Adapted from the Makefile in the Istio repo.
#
# NOTE. This script is only useful for debugging and understanding how
# the Istio build works. In general, you're better off using Istio's
# own Makefile, e.g.
#
#     $ pushd "${ISTIO_REPO}" && BUILD_WITH_CONTAINER=1 make gen
#

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
SCRIPTSDIR="${ROOTDIR}/scripts"

source "${SCRIPTSDIR}/env.sh"


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
    READLINK_FLAGS="-f"
elif [ "${LOCAL_OS}" = Darwin ]
then
    TARGET_OS=darwin
    READLINK_FLAGS=""
else
    echo "Unsupported OS: ${LOCAL_OS}"
    exit 1
fi


IMG="gcr.io/istio-testing/build-tools:master-2019-11-14T12-01-13"
echo "Building with Istio build container: ${IMG}"

# Where to output build artifacts in the container. 
TARGET_OUT=/work/out/${TARGET_OS}_${TARGET_ARCH}

# Determine the timezone across various platforms to pass into the
# docker run operation. This operation assumes zoneinfo is within
# the path of the file.
TIMEZONE=$(readlink ${READLINK_FLAGS} /etc/localtime | sed -e 's/^.*zoneinfo\///')

# Determine the docker.push credential bind mounts.
echo "Using docker credential directory ${HOME}/.docker"
DOCKER_CREDS_MOUNT="--mount type=bind,source=${HOME}/.docker,destination=/config/.docker,readonly"

DOCKER_SOCKET_MOUNT="-v /var/run/docker.sock:/var/run/docker.sock"

docker run -t -i --sig-proxy=true --rm \
	-u ${UID} \
	-e IN_BUILD_CONTAINER="1" \
	-e TZ="${TIMEZONE}" \
	-e TARGET_ARCH="${TARGET_ARCH}" \
	-e TARGET_OS="${TARGET_OS}" \
	-e TARGET_OUT="${TARGET_OUT}" \
	-e USER="${USER}" \
	-v /etc/passwd:/etc/passwd:ro \
	${DOCKER_SOCKET_MOUNT} \
	--mount type=bind,source="${ISTIO_REPO}",destination="/work" \
	--mount type=volume,source=go,destination="/go" \
	--mount type=volume,source=gocache,destination="/gocache" \
	${DOCKER_CREDS_MOUNT} \
	-w /work \
	${IMG} \
    $@

# go build -o "${GOBIN}/mixgen" mixer/tools/mixgen/main.go
# export REPO_ROOT=/work
# bin/mixer_codegen.sh -a mixer/adapter/orionadapter/config/config.proto -x "-s=false -n orionadapter -t authorization"
# OK

# bin/mixer_codegen.sh -a /xxx/orionadapter/config/config.proto -x "-s=false -n orionadapter -t authorization"
# fail: dir must be relative to REPO_ROOT

# bin/mixer_codegen.sh -t mixer/adapter/keyval/template.proto
# OK
# bin/mixer_codegen.sh -t mixer/adapter/_kv/template.proto
# diff: dir name in gen code

# None of above Docker options needed for code gen, just:
# docker run -t -i --rm \
# --mount type=bind,source=xxx/urad,target=/work/mixer/adapter/urad \
# boost/orionadapter/codegen \
# -t mixer/adapter/urad/template.proto

docker run -t -i --rm \
	--mount type=bind,source=/Users/andrea/go/src/orchestracities/boost/orionadapter,target=/work/mixer/adapter/orionadapter \
	boost/orionadapter/codegen \
	-a mixer/adapter/orionadapter/config/config.proto \
	-x "-s=false -n orionadapter -t authorization"