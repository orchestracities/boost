Mixer Adapter Code Generation
-----------------------------
> Mixing esoteric substances in an occult ceremony.

Notes about the various approaches to Istio code generation I struggled
with, specifically how to generate config and templates for Mixer
out-of-process adapters. Contents:

1. *Lay of the land*. Overview of how code generation works for existing
   Mixer adapters, i.e. for code that sits in the Istio repo.
2. *Mixer codegen nitty-gritty*. How to use Mixer codegen facilities to
   generate artifacts for your own adapters **inside** an Istio repo clone.
3. *Quest for the philosopher's stone*. Ways to generate artifacts for
   adapter code sitting **outside** of the Istio repo. Don't hold your
   breath for the silver bullet.

Take home:

* Code generation can get pretty messy for custom adapters. Our current
  approach is to use a build container with the Istio toolchain and
  `1.4.2.` repo in it where we mount our adapter dir to generate the
  config and template stuff we need. Have a look at `scripts/codegen.sh`.


### Lay of the land

Mixer (out-of-process) adapters have two components that require code
generation: configuration and gRPC scaffolding---a.k.a. templates. In
both cases, the inputs to code generation are Protobuf files that define
data structures from which to derive Go code, Kubernetes (or any other
supported framework) resource descriptors and component docs.
If that's Greek to you, you can try reading [this post][adapter-model]
from the Istio blog about adapters and all that jazz. In the Istio
code base, each adapter gets a dir below `mixer/adapter`---e.g. the
List adapter sits in `mixer/adapter/list`. In each adapter dir, you'll
usually find a Protobuf file (`config/config.proto`) defining adapter
configuration whereas adapter templates get a separate dir under
`mixer/template`---e.g. the List adapter's template is in
`mixer/template/listentry`. The below diagram shows typical code
generation inputs and outputs.

![alt text](./adapter.code-gen.png)

But hang on, how does it actually happen? There's a script in the Istio
repo

    bin/mixer_codegen.sh

that each and every adapter calls indirectly, through `go generate`
code annotations, to produce config and template derived artifacts.


### Mixer codegen nitty-gritty

It looks like `bin/mixer_codegen.sh` was probably never meant to be
(ab-)used (?) for generating code from files sitting **outside** of
the Istio code base. Here are some reasons why I think that is:

1. Input config and template file paths have to be relative to the
   Istio repo root dir. See [line 23][codegen.sh-l23], [28][codegen.sh-l28],
   [51][codegen.sh-l51] and [59][codegen.sh-l59].
2. `mixer_codegen.sh` calls another script, `bin/protoc.sh`, which
   collects a few Istio import paths to pass down to `protoc`---Protobuf's
   code generator---but this won't work if the script gets called
   outside of the Istio repo. See lines [24][protoc.sh-l24] and
   [28][protoc.sh-l28].
3. `mixer_codegen.sh` calls `mixgen`, Mixer's own code generation tool
   that I don't think is part of any Istio release? (It gets built by
   the [Makefile][mk.core-l394] just before kicking off Go code generation...)

Long story short: to generate adapter artifacts out of your Protobuf
config and template files, you'll have to stick them in an Istio repo
clone and then run code generation in there. With your files in the
Istio repo, there's two ways to get the job done that I know of.

#### Hassle-free codegen
You can run code generation in Istio's build container

    $ cd /path/to/your/istio/clone
    $ BUILD_WITH_CONTAINER=1 make gen

Easy peasy lemon squeezy! This saves you from the hassle of installing
the Istio build toolchain locally, but your artifacts won't be there
in a snap---go for coffee.

#### Fast codegen
Need a quick turnaround? Run `mixer_codegen.sh` directly on your file,
e.g.

    $ cd /path/to/your/istio/clone
    $ bin/mixer_codegen.sh -t mixer/adapter/uradapter/template.proto

Not so fast. There are a couple of things to sort out before you can
actually do that. Since `mixer_codegen.sh` expects `mixgen` to be in
the path, you should

    $ cd /path/to/your/istio/clone
    $ go build -o "${GOBIN}/mixgen" mixer/tools/mixgen/main.go

Take a peek at Istio's [Makefile][mk.core-l394] :-) So that's a no-brainer.
The hairy bit is to figure out what external tools/libs to install
on your box. For me the following setup worked decently on MacOS.
First install the Protobuf compiler (version >= 3.10):

    $ brew install protobuf

then the Go Protobuf plugins:

    $ go get -u github.com/golang/protobuf/protoc-gen-go
    $ go get -u github.com/pseudomuto/protoc-gen-doc/cmd/protoc-gen-doc

and finally the Gogo Protobuf plugins:

    $ go get github.com/gogo/protobuf/proto
    $ go get github.com/gogo/protobuf/protoc-gen-gogoslick
    $ go get github.com/gogo/protobuf/gogoproto

There's a snag though. With this setup, docs generation didn't work
for me, `mixer_codegen.sh` would moan about `protoc-gen-docs` not
being there as if `protoc` was looking for `protoc-gen-docs` instead
of `protoc-gen-doc`---note the extra 's'!---which is the one in the
`PATH` and seems to be the official docs plugin binary.

I was able to run the script only after adding `-d false` to skip docs
generation as in:

    $ bin/mixer_codegen.sh \
        -a orionadapter/config/config.proto \
        -x "-s=false -n orionadapter -t authorization" \
        -d false


### Quest for the philosopher's stone

So by the sound of it, the easiest way to develop an Istio adapter is
to have all your code in the Istio repo. This is the approach tacitly
assumed by the [Mixer Out of Process Adapter Walkthrough][adapter-walkthru]
article on the Istio wiki. But there may be good reasons why you'd
rather keep your code in a separate repo. What then?

#### Istio's build container
Like I said earlier, one option is to use Istio's build container but
we'll need a clone of the Istio repo to make it work. `gen-config.sh`
follows this approach to generate config artifacts out of `config.proto`
in `orionadapter/config`. That's how it works:

1. Our `orionadapter/config/version.go` contains a Go code generation
   instruction to run Mixer's `mixer_codegen.sh`.
2. We clone the Istio repo, then we sneakily copy our adapter files
   under the Mixer tree where Istio codegen expects them to be.
3. We run the Istio build container in the Istio repo to generate the
   goodies and finally copy the generated files back over to our repo.

Yuck! Also keep in mind the script runs `make gen` which will try
generating the whole shebang, i.e. it'll pick up all `proto` files
in the Istio repo. Each run took approx 12 mins on my MacBook Pro
13...

#### Local toolchain
`gen-config.local.sh` is an alternative to `gen-config.sh` that uses
a local Istio codegen toolchain to generate the goodies in the blink
of an eye. That's how it works:

1. You've installed the Istio codegen toolchain as detailed earlier.
2. We build the missing binary in the toolchain, `mixgen`, and
   add it to the `PATH`.
3. We clone the Istio repo, then we sneakily copy our adapter files
   under the Mixer tree where Istio codegen expects them to be.
4. We run the config codegen command directly in the Istio repo clone
   using `bin/mixer_codegen.sh`.
   (Same command as in  `orionadaper/config/version.go`.)
5. We finally copy the generated files back over to our repo.

So this is fast, but you'll have to install the codegen toolchain
and I still haven't managed to get this setup 100% right---see earlier
comments about docs generation failing!

#### Own build container
We also rolled out our own build container to get fast codegen without
a local toolchain. The container comes with the entire Istio toolchain
and a shallow clone of the Istio repo at the revision of tag `1.4.2`.
Have a look at `container/codegen.dockerfile`. The `codegen.sh` script
in the `scripts` dir takes care of building it and then mounts our
local `orionadapter` dir on the container's Istio repo tree where
Istio codegen expects it to be. Then it runs code generation only
on our `proto` files.

That's all well and good but we winded up with a `2GB` container and
we'll still have to cope with maintenance headaches as in the other
approaches---think moving `proto` files around...




[adapter-model]: https://istio.io/blog/2017/adapter-model/
    "Mixer Adapter Model"
[adapter-walkthru]: https://github.com/istio/istio/wiki/Mixer-Out-Of-Process-Adapter-Walkthrough
    "Mixer Out of Process Adapter Walkthrough"
[codegen.sh-l23]: https://github.com/istio/istio/blob/1.4.2/bin/mixer_codegen.sh#L23
    "Mixer Codegen Script - Line 23"
[codegen.sh-l28]: https://github.com/istio/istio/blob/1.4.2/bin/mixer_codegen.sh#L28
    "Mixer Codegen Script - Line 28"
[codegen.sh-l51]: https://github.com/istio/istio/blob/1.4.2/bin/mixer_codegen.sh#L51
    "Mixer Codegen Script - Line 51"
[codegen.sh-l59]: https://github.com/istio/istio/blob/1.4.2/bin/mixer_codegen.sh#L59
    "Mixer Codegen Script - Line 59"
[mk.core-l394]: https://github.com/istio/istio/blob/1.4.2/Makefile.core.mk#L394
    "Istio Core Makefile - Line 394"
[protoc.sh-l24]: https://github.com/istio/istio/blob/1.4.2/bin/protoc.sh#L24
    "Mixer protoc Wrapper Script - Line 24"
[protoc.sh-l28]: https://github.com/istio/istio/blob/1.4.2/bin/protoc.sh#L28
    "Mixer protoc Wrapper Script - Line 28"
