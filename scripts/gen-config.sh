#!/usr/bin/env bash

# Generate Mixer config code and YAML for our adapter.
# Note the way we're doing this is kinda insane, but it looks like there's
# no easy way around it. First we clone the Istio repo, then we sneakily
# copy our adpater files under the Mixer tree where Istio code gen expects
# them to be. (See: https://github.com/istio/istio/wiki/Mixer-Out-Of-Process-Adapter-Walkthrough)
# Then we run code gen in the Istio repo and finally copy the generated files
# back over to our repo. Yuck!
# We set the BUILD_WITH_CONTAINER flag to tell Istio code gen to use a 
# container to build stuff rather than a local tool chain. The downside
# is that it takes a loooong time to do a build. For what is worth, I
# tried to generate those files directly, running from our repo base dir:
#
#     REPO_ROOT=$ISTIO_REPO $ISTIO_REPO/bin/mixer_codegen.sh \
#     -a orionadapter/config/config.proto \
#     -o ./orionadapter/config/ \
#     -x "-s=false -n orionadapter -t authorization"
# 
# But that wouldn't work cos mixer_codegen.sh prepends the Isto repo path
# to the -a arg value---see how -a gets handled on line 49. So it looks like
# we're forced to copy our files over to the Istio repo. Now, after copying
# the files over, in principle we should be able to run the above command,
# provided we have all tools required by code gen in our path. I've managed
# to narrow down the deps to:
#
# * go protocol buffers plugins:
#     - go get -u github.com/golang/protobuf/protoc-gen-go
#     - go get -u github.com/pseudomuto/protoc-gen-doc/cmd/protoc-gen-doc
# * gogo protocol buffers plugins:
#     - go get github.com/gogo/protobuf/proto
#     - go get github.com/gogo/protobuf/protoc-gen-gogoslick
#     - go get github.com/gogo/protobuf/gogoproto
# 
# with those tools installed in GOPATH/bin I was able to run the script,
# after adding '-d false' to skip docs generation. In fact, that fails
# since code gen is looking for `protoc-gen-docs` instead of `protoc-gen-doc`---
# note the extra 's'! Anyhoo, even after I went past that, the script bombed
# out cos `mixgen` was nowhere to be found. Supposedly this is the last hurdle,
# but didn't have time to work it out so I just left it.

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
