#!/usr/bin/env bash

# Generate Mixer artifacts (Go code, cloud infra defs, docs) for our
# adapter.
#
# That's how it works:
#
# 1. We build a container with the entire Istio toolchain and a shallow
#    clone of the Istio repo at the revision of tag `1.4.2`.
# 2. Then we mount our local `orionadapter` dir on the container's Istio
#    repo tree where Istio codegen expects it to be.
# 3. Finally, we run code generation only on our `proto` files.
#
# Read more about adapter code generation in `docs/codegen.md`.
#

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
ADAPTERDIR="${ROOTDIR}/orionadapter"

echo "Building toolchain Docker image."
pushd "${ROOTDIR}" && \
    docker build -t boost/orionadapter/codegen \
                 -f container/codegen.dockerfile .

echo "Generating adapter config artifacts."
docker run -t -i --rm \
    --mount type=bind,source="${ADAPTERDIR}",target=/work/mixer/adapter/orionadapter \
    boost/orionadapter/codegen \
    -a mixer/adapter/orionadapter/config/config.proto \
    -x "-s=false -n orionadapter -t authorization"
