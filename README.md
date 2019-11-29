BOOST4.0 EIDS FIWARE CIM Connector
==================================
> Messin' about w/ Istio to bridge EIDS and Fiware.


### The basic idea

An Istio custom adapter to intercept and validate IDS-DTH tokens. If the
token is valid the requests gets forwarded on to Orion, else it winds up
in the sink of no return :-)


### Dev env

* git
* go >= 1.13
* add $GOPATH/bin (usually ~/go/bin) to your $PATH
* protobuf compiler >= 3.10 (brew install protobuf)
* go protocol buffers plugins:
    - go get -u github.com/golang/protobuf/protoc-gen-go
    - go get -u github.com/pseudomuto/protoc-gen-doc/cmd/protoc-gen-doc
* gogo protocol buffers plugins:
    - go get github.com/gogo/protobuf/proto
    - go get github.com/gogo/protobuf/protoc-gen-gogoslick
    - go get github.com/gogo/protobuf/gogoproto
* minikube >= 1.5 (or docker k8s?)
* kubectl >= 1.7
* docker


### Taking it for a spin

For the brave:

    $ sh scripts/gen-config.sh
    $ go build ./...
    $ sh scripts/make-mix.sh
    $ sh scripts/populate-testdata.sh
    $ go run orionadapter/main.go 43210

Now our custom adapter is running and waiting for the Mixer server to
hook up. Bring up the Mixer server in a new terminal:

    $ sh scripts/run-mixer.sh

The script will only run on MacOS but it's trivial to tweak it to make
it work on other OSes too. Now you're ready to use the Mixer client to
send an IDS-DTH token to the Mixer server. Open a new terminal and run:

    $ sh scripts/send-token.sh my.fat.jwt

(This script too only works on MacOS but it's easy to port to other OSes.)

At this point you should be able to see a status OK being returned.
The adapter checks if the token you send is the same as the one in the
`ids_dth_expected_token` adapter config field whose value, as you've guessed
already, is "my.fat.jwt"---edit `testdata/sample_operator_cfg.yaml` to
change it to something else. 

If you call the script with a different token:

    $ sh scripts/send-token.sh this.should.fail

you should get a fat permission denied back.


### Local cluster deployment

Now on to something even more adventurous. We're going to run a Kubernetes
local cluster using Minikube, deploy Istio with its demo profile, and run
our adapter in it. Brace! Brace!

##### Deploying Istio

After installing Minikube, download the Istio release and install the demo
profile. Here's the short version, assuming you've already installed Minikube:

    $ cd ~
    $ minikube start --memory=16384 --cpus=4
    $ kubectl config use-context minikube
    $ curl -L https://istio.io/downloadIstio | sh -
    $ cd istio-*
    $ export PATH=$PWD/bin:$PATH
    # ...ideally you should add the above to your Bash profile.
    $ istioctl manifest apply --set profile=demo

Long version:

- https://istio.io/docs/setup/getting-started/

##### Adapter image

Now let's build our adapter's Docker image and then make Minikube use
that local image. In a new terminal:

    $ cd ~/go/src/orchestracities/boost/
    $ eval $(minikube docker-env)
    $ sh scripts/make-image.sh

The above basically stashes away our image in Minikube's own local Docker
registry so that it can be fetched from there instead of trekking all
the way to DockerHub. This article explains how the trick works:

- https://dzone.com/articles/running-local-docker-images-in-kubernetes-1

##### Deploying the adapter

**TODO**

k8s apply---see `deployment/orion_adapter_service.yaml`.

see:
- https://github.com/salrashid123/istio_custom_auth_adapter


### Current status

All the adapter scaffolding is in place, we just need to implement the
`validateToken` function by porting the Ballerina code over which shouldn't
be too hard. Coming soon:

* mutual TLS (almost there!)
* K8s + Istio + adapter local and cloud deployment (in progress)
* Istio gateway / virtual service to handle IDS / Fiware message translation
  (will take blood, sweat and tears :-)
