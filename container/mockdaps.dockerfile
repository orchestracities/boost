#
# Gloriously lifted & tweaked from:
# - https://dev.to/ivan/go-build-a-minimal-docker-image-in-just-three-steps-514i
#
# Build from repo root dir with:
#
#     $ docker build -t boost/mockdaps -f container/mockdaps.dockerfile .
#
FROM golang:1.13.4-stretch AS builder

# Set up Go compiler env.
ENV CGO_ENABLED=0
# NOTES.
# 1. CGO. We're not using any C libs from our Go code so we disable CGO.
# Strangely enough, without doing this explicitly, I get this error when
# running the container:
#
#     $ docker run -t -i boost/mockdaps
#     standard_init_linux.go:211: exec user process caused "no such file or directory"
#

# Create build dir.
WORKDIR /build

# We have no `go.mod` for the mock DAPS server at the moment but the code
# only depends on Go built-in libs. We could use the adpater's `go.mod`
# but that'd be such a waste of space and build time since it pulls down
# one gazillion deps. Instead we're going to have a plain old Go build
# without modules, so we need to set up our GOPATH.
ENV GOPATH=/build

# Cache modules - those don't change so often.
# COPY go.mod .
# COPY go.sum .
# RUN go mod download

# Copy the code necessary to build the mock DAPS server.
RUN mkdir -p src/github.com/orchestracities/boost
COPY ./mockdaps src/github.com/orchestracities/boost/mockdaps

# Build it.
RUN go build -o bin/mockdaps github.com/orchestracities/boost/mockdaps

# Create a /dist folder containing just the files necessary for runtime.
# Later, it will be copied as the / (root) of the output image.
WORKDIR /dist
RUN cp /build/bin/mockdaps ./

# Copy or create other directories/files your app needs during runtime.
# E.g. this example uses /data as a working directory that would probably
#      be bound to a perstistent dir when running the container normally
RUN mkdir /data

# Create the minimal runtime image.
FROM scratch

# TODO: review stuff below and get a grip on how the heck it actually works.
# Oh, and while you're at it, parameterize the adapter port!

COPY --chown=0:0 --from=builder /dist /

# Set up the app to run as a non-root user inside the /data folder
# User ID 65534 is usually user 'nobody'.
# The executor of this image should still specify a user during setup.
COPY --chown=65534:0 --from=builder /data /data
USER 65534
WORKDIR /data

ENTRYPOINT ["/mockdaps"]
CMD [ "44300" ]
EXPOSE 44300
