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

**Note**. *Local repo location*. I'm going to assume you've cloned this
repo in `$GOPATH/src/orchestracities/boost`, i.e. you're sticking to Go's
customary layout rules.

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
    $ kubectl -n istio-system edit cm istio
    # ...set disablePolicyChecks to false, save & exit
    $ kubectl label namespace default istio-injection=enabled

Long version:

- https://istio.io/docs/setup/getting-started/
- https://istio.io/docs/setup/additional-setup/sidecar-injection/#automatic-sidecar-injection
- https://istio.io/docs/tasks/policy-enforcement/enabling-policy/

**Note**. *Policy Enforcement*. The docs say the `demo` profile should enable
it (i.e. set `disablePolicyChecks` to `false`) but it doesn't nor does it
work to specify that option at installation time which is why you'll have
to manually edit the K8s config after applying the Istio `demo` profile.

##### Adapter image

Now let's build our adapter's Docker image and then make Minikube use
that local image. In a new terminal:

    $ cd $GOPATH/src/orchestracities/boost/
    $ eval $(minikube docker-env)
    $ sh scripts/make-image.sh
    $ exit

The above basically stashes away our image in Minikube's own local Docker
registry so that it can be fetched from there instead of trekking all
the way to DockerHub. This article explains how the trick works:

- https://dzone.com/articles/running-local-docker-images-in-kubernetes-1

##### Deploying a dummy target service

Our adapter is supposed to process attributes of messages directed to
Orion. For the sake of testing though, there's no need to deploy Orion
& friends, any dummy HTTP service will do but a better option is `httpbin`
which can echo back HTTP messages too---this will come in handy to e.g.
check the adapter removes IDS token headers before the HTTP request gets
passed down to the target service.

    $ cd $GOPATH/src/orchestracities/boost/
    $ kubectl apply -f deployment/httpbin_service.yaml
    $ kubectl apply -f deployment/ingress_routing.yaml

Now you should wait a bit until `httpbin` and all Istio services/pods are
alive & kicking. Then you should be able to see what HTTP headers the
`httpbin` service gets to see when you `curl` a request:

    $ source scripts/cluster-url.sh
    $ curl -v "$BASE_URL"/headers

Should we have some fun with HTTP headers?

    $ curl -v "$BASE_URL"/headers \
        -H this-wont-be-dropped:cool-bananas! \
        -H ids-dth:will-be-dropped

The `ids-dth` header gets dropped from the HTTP request before it
gets to the `httpbin` service (see `ingress_routing.yaml`) whereas
any other header gets passed on.

##### Deploying the adapter

Time to plonk in our token-buster baton wielding copper.

    $ kubectl apply -f deployment/template.yaml
    $ kubectl apply -f deployment/orionadapter.yaml
    $ kubectl apply -f deployment/orion_adapter_service.yaml
    $ kubectl apply -f deployment/sample_operator_cfg.yaml

Check the Mixer made friends with our boy:

    $ kubectl -n istio-system logs \
        $(kubectl -n istio-system get pods -lapp=telemetry \
            -o jsonpath='{.items[0].metadata.name}') | grep orion

(You should see: `grpcAdapter	Connected to: orionadapterservice:43210`)

See if we can get still away with an invalid token...

    $ curl -v "$BASE_URL"/headers \
        -H ids-dth:catch.me.copper

You should get back a fat 403 with a message along the lines of:

    PERMISSION_DENIED:h1.handler.istio-system:Unauthorized: invalid JWT token

Like I said earlier, the adapter checks if the token you send is the same
as the one in the `ids_dth_expected_token` adapter config field which is
set to "my.fat.jwt"---see `deployment/sample_operator_cfg.yaml`. What
happens if we send a valid token then?

    $ curl -v "$BASE_URL"/headers \
        -H ids-dth:my.fat.jwt

Well, the request should go through to the target `httpbin` service
which should reply with the HTTP headers it gets to see and there
should be no `ids-dth` header in the returned list since our routing
chops that head(-er) off before sending the request on to `httpbin`.
Happy days!

##### Cleaning up

**TODO**

    $ kubectl delete gateway httpbin-gateway
    $ kubectl delete virtualservice httpbin
    $ kubectl delete service httpbin
    $ kubectl delete deployment httpbin
    $ ...

or simply:

    $ minikube delete

if you have plenty of time in your hands and don't mind the schlep to
redo everything from a clean slate!


### Current status

* adapter scaffolding (done)
* token validation against configured value (done, see `validateToken`)
* dropping of token header before forwarding message to Orion (done)
* response token header injection using a configured token value (in progress)
* K8s + Istio + adapter local and cloud deployment (done)
* mutual TLS (almost there!)
* Istio gateway / virtual service to handle IDS / Fiware message translation
  (will take blood, sweat and tears :-)
