#!/usr/bin/env bash

# Generate Mixer config code and cloud infrastructure resource defs (YAML
# files) for our adapter. No docs generation.
#
# This script is an alternative to `gen-config.sh` that uses a local
# Istio codegen toolchain to generate the goodies in the blink of an
# eye. That's how it works:
#
# 1. You've installed the Istio codegen toolchain as documented in
#    `docs/codegen.md`.
# 2. We build the missing binary in the toolchain, `mixgen`, and
#    add it to the `PATH`.
# 3. We clone the Istio repo, then we sneakily copy our adapter files
#    under the Mixer tree where Istio codegen expects them to be.
# 4. We run the config codegen command directly in the Istio repo clone
#    using `bin/mixer_codegen.sh`.
#    (Same command as in  `orionadaper/config/version.go`.)
# 5. We finally copy the generated files back over to our repo.
#
# Read more about adapter code generation in `docs/codegen.md`.
#

set -e
SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
SCRIPTSDIR="${ROOTDIR}/scripts"
ADAPTERDIR="${ROOTDIR}/orionadapter"

source "${SCRIPTSDIR}/env.sh"

sh "${SCRIPTSDIR}/get-istio-src.sh"


MIXGEN_BINDIR=/tmp/bin

echo "Building mixgen command in ${MIXGEN_BINDIR}"
pushd "${ISTIO_REPO}" && \
    go build -o "${MIXGEN_BINDIR}/mixgen" mixer/tools/mixgen/main.go


export REPO_ROOT="${ISTIO_REPO}"
export PATH="${MIXGEN_BINDIR}:${PATH}"

echo "Merging our adapter code into Mixer source."
cp -r "${ADAPTERDIR}" "${MIXER_REPO}/adapter/"

echo "Generating adapter config in Mixer repo."
"${ISTIO_REPO}/bin/mixer_codegen.sh" \
    -a mixer/adapter/orionadapter/config/config.proto \
    -x "-s=false -n orionadapter -t authorization" \
    -d false

# NOTE. Docs generation.
# We skip it (`-d false`) since otherwise `mixer_codegen.sh` would moan
# about `protoc-gen-docs` not being there as if `protoc` was looking for
# `protoc-gen-docs` instead of `protoc-gen-doc`---note the extra 's'!---
# which is the one we have in the PATH and seems to be the official
# docs plugin binary.

echo "Copying back generated adapter config."
cp -a "${MIXER_REPO}/adapter/orionadapter/config/." "${ADAPTERDIR}/config/"
