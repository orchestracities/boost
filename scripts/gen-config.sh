#!/usr/bin/env bash

# Generate Mixer config code for our adapter as well as acompanying
# cloud infrastructure resource defs (YAML files) and docs.
#
# That's how it works:
#
# 1. Our `orionadapter/config/version.go` contains a Go code generation
#    instruction to run Mixer's `mixer_codegen.sh`.
# 2. We clone the Istio repo, then we sneakily copy our adapter files
#    under the Mixer tree where Istio codegen expects them to be.
# 3. We run codegen in the Istio repo and finally copy the generated
#    files back over to our repo.
#
# Yuck! It's kinda insane, but that's how you're supposed to do it
# according to:
#
# - https://github.com/istio/istio/wiki/Mixer-Out-Of-Process-Adapter-Walkthrough
#
# Read more about it in our `docs/codegen.md`.
#
# In step 3, we set the BUILD_WITH_CONTAINER flag to tell Istio codegen
# to use a container to build stuff rather than a local toolchain. The
# downside is that it takes a loooong time to do a build.
#

set -e

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
SCRIPTSDIR="${ROOTDIR}/scripts"
ADAPTERDIR="${ROOTDIR}/orionadapter"

source "${SCRIPTSDIR}/env.sh"

sh "${SCRIPTSDIR}/get-istio-src.sh"

echo "Merging our adapter code into Mixer source."
cp -r "${ADAPTERDIR}" "${MIXER_REPO}/adapter/"

echo "Generating adapter config in Mixer repo."
set +e
pushd "${ISTIO_REPO}" && BUILD_WITH_CONTAINER=1 make gen
set -e
# NOTES.
# 1. BUILD_WITH_CONTAINER=1 tells Isto code gen to use a build container
# instead of a local toolchain.
# 2. `make gen` will fail at the 'tidy-go' Make target. This is because the
# target runs `go mod tidy` which will try to resolve our imports from the
# code we copied over, e.g. github.com/orchestracities/boost/orionadapter/
# but the code isn't in GitHub yet. While this may work when we publish the
# code, for the time being we can safely ignore the failure since it happens
# **after** all files get generated. Hence the `set +e` / `set -e` above.

echo "Copying back generated adapter config."
cp -a "${MIXER_REPO}/adapter/orionadapter/config/." "${ADAPTERDIR}/config/"
