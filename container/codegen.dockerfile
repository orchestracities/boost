#
# A container for Istio code generation.
# It comes with the Istio build toolchain so we can generate Istio code
# for the adapter without having to install locally the Istio build env
# & deps.
#
# The container's entry point is the Mixer's code generation script:
# `bin/mixer_codegen.sh` and there's a `/work` directory with the `1.4.2`
# Istio source so that we can easily generate adapter code by mounting
# the directory containing our Protobuf files on `/work/mixer/adapter`
# as in the below example
#
#     $ docker run -t -i --rm \
#         --mount type=bind,source=/path/to/urad,target=/work/mixer/adapter/urad \
#         boost/orionadapter/codegen \
#         -t mixer/adapter/urad/template.proto
#
# which generates the goodies (template code, cloud infra defs, docs)
# out of `/path/to/urad/template.proto` and then puts the generated
# stuff in the same dir: `/path/to/urad`.
#
# Build this container from this repo's root dir with:
#
#     $ docker build -t boost/orionadapter/codegen \
#           -f container/codegen.dockerfile .
#

# Start from the Istio build tools image. It contains 99% of what we need,
# so we can just add a few tweaks to make code gen sweeter.
FROM gcr.io/istio-testing/build-tools:release-1.4-2019-11-12T19-29-46

# NOTE. Base Image.
# Same as the one in Istio's `Makefile` at the revision of tag `1.4.2`:
# - https://github.com/istio/istio/blob/1.4.2/Makefile#L59
# Below, we also pull the Istio repo at `1.4.2`. If you change the Istio
# repo revision down below, make sure to also update the base image with
# the container image declared in `Makefile` at the new Git revision.

WORKDIR /

# Shallow clone the Istio repo. We need some of the files in there to
# make code gen work---see `bin/protoc.sh`. We're going to clone in the
# `/work` dir to keep the same dir layout as in the build container that
# gets started by Istio's `Makefile`---see Docker mount option there.
RUN git clone -b '1.4.2' --depth 1 https://github.com/istio/istio.git work

# NOTE. Repo revision.
# We pull the revision of tag `1.4.2`. If you want to change this, make
# sure to also update this Docker file's base image---see earlier note.

WORKDIR /work
# NOTE. Convenience setting?
# Istio's `Makefile` starts the build container with a working dir of
# `/work`, after mounting the Istio repo on it. Not sure if some of
# Istio's buid scripts expect this kind of set up or it was done this
# way just for the extra convenience.

# Build Istio's `mixgen` tool and make it available in the `PATH`.
RUN go build -o "${GOBIN}/mixgen" mixer/tools/mixgen/main.go

# NOTES.
# 1. Environment. The base image comes with Go, `GOBIN` set to `/gobin`
# and `GOBIN` in the `PATH`.
# 2. Build command. Same as in Istio's `Makefile.core.mk`---see `go-gen`
# target.
# 3. Go deps. The build commands pulls down most of them I guess. In
# fact, before this step the container's `/go` and `/gocache` dirs
# were empty, but after the build completed they had loads of files
# in them. Then code generation works without having to mount local
# Go dirs as it's done in Istio's Makefile:
#
#    ...
#    --mount type=volume,source=go,destination="/go" \
#    --mount type=volume,source=gocache,destination="/gocache"
#    ...

ENV IN_BUILD_CONTAINER=1
# Not sure what this is for but that's one env var Istio's `Makefile`
# slots in when launching the build container.

ENV REPO_ROOT=/work
# We need this for code gen. In fact, `bin/mixer_codegen.sh` calls
# `bin/protoc.sh` which expects `REPO_ROOT` to point to the local
# Istio repo root.

ENTRYPOINT [ "bin/mixer_codegen.sh" ]
