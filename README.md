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
* minikube >= 1.5
* kubectl >= 1.7
* docker

**Note**. *Local repo location*. I'm going to assume you've cloned this
repo in `$GOPATH/src/orchestracities/boost`, i.e. you're sticking to Go's
customary layout rules.

### Taking it for a spin

For the brave:

    $ sh scripts/codegen.sh
    $ go build ./...
    $ sh scripts/make-mix.sh
    $ sh scripts/populate-testdata.sh
    $ go run orionadapter/main.go 43210

**Note**. `make-mix.sh`. It only runs on MacOS and Linux but it shouldn't
be impossible to tweak it to make it work on other OSes too.

Now our custom adapter is running and waiting for the Mixer server to
hook up. Bring up the Mixer server in a new terminal:

    $ sh scripts/run-mixer.sh

Next up is our own test DAPS service. Open up a new term and:

    $ go run mockdaps/main.go 44300

Finally you're ready to use the Mixer client to send an IDSA header to the
Mixer server. Open a new terminal and run:

    $ export MY_FAT_JWT=eyJhbGciOiJSUzI1NiJ9.e30.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw
    $ sh scripts/send-token.sh "${MY_FAT_JWT}"

At this point you should be able to see a status of `OK` being returned.
Hang on a minute! What's just happened? The script takes a JWT token as
input, puts it in an IDSA template header and sends it to the Mixer which,
in turn, passes the IDSA header on to the adapter. On getting the header
value, the adapter verifies the token's RSA 256 signature using the public
key in its config---see `_output_/testdata/sample_operator_cfg.yaml`.
Then it goes on to requesting an ID token from our local DAPS test
service---again have a look at `_output_/testdata/sample_operator_cfg.yaml`.
In the DAPS terminal, you should be able to see the adapter hitting
DAPS with a request for an ID token.

If you call the script with an invalid token:

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
    $ export PATH="${PWD}/bin:${PATH}"
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
        -H header:will-be-dropped

The `header` header gets dropped from the HTTP request before it
gets to the `httpbin` service (see `ingress_routing.yaml`) whereas
any other header gets passed on. Uh? `header` header? Yep, you heard
right, the IDSA header is aptly called `header` :-)

**Note**. *Header removal*. We disabled this at the moment since it
gets in the way of token validation---see #11.
So, much to your disappointment, the above won't work---i.e. the IDSA
header won't get dropped.

##### Deploying the adapter

Time to plonk in our token-buster baton wielding copper.

    $ sh scripts/populate-deployment.sh
    $ kubectl apply -f deployment/template.yaml
    $ kubectl apply -f deployment/orionadapter.yaml
    $ kubectl apply -f deployment/orion_adapter_service.yaml
    $ kubectl apply -f deployment/sample_operator_cfg.yaml

Check the Mixer made friends with our boy:

    $ kubectl -n istio-system logs \
        $(kubectl -n istio-system get pods -lapp=telemetry \
            -o jsonpath='{.items[0].metadata.name}') | grep orion

(You should see: `grpcAdapter	Connected to: orionadapterservice:43210`)

See if we can still get away with an invalid token...

    $ curl -v "$BASE_URL"/headers \
        -H header:catch.me.copper
        #  ^ the IDSA header is actually called "header"

You should get back a fat 403 with a message along the lines of:

    PERMISSION_DENIED:h1.handler.istio-system:unauthorized: invalid JWT data

Like I said earlier, the adapter verifies the JWT you send is valid---see
`deployment/sample_operator_cfg.yaml`. What happens if we send a valid
token then? Here's a valid JWT signed with the private key in the config.

    $ export MY_FAT_JWT=eyJhbGciOiJSUzI1NiJ9.e30.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw

Now we can use this convenience script to stick it into an IDSA header:

    $ export HEADER_VALUE=$(sh scripts/idsa-header-value.sh "${MY_FAT_JWT}")

and send the header with

    $ curl -v "$BASE_URL"/headers \
        -H "header:${HEADER_VALUE}"

The request should go through to the target `httpbin` service
which should reply with the HTTP headers it gets to see and there
should be no IDSA header in the returned list since our routing
chops that head(-er) off before sending the request on to `httpbin`.
(But see note above about header removal!) You should also be able to
spot a `fiware-ids-server-token` among the response headers: this is
where we plonk in the IDS server token we generate. What you see on
your terminal should be similar to:

    HTTP/1.1 200 OK
    ...
    fiware-ids-server-token: generated.server.token

    {
        "headers": {
            "Accept": "*/*",
            "Content-Length": "0",
            "Host": "192.168.64.4:30072",
            "Header": "ewoJIkB0eXBlIjogIm...(same as $HEADER_VALUE)...",
            "User-Agent": "curl/7.64.1",
            "X-B3-Parentspanid": "aca5010612a10730",
            "X-B3-Sampled": "1",
            "X-B3-Spanid": "04ab3dca9ad2cd85",
            "X-B3-Traceid": "4f8e24520e1dac36aca5010612a10730",
            "X-Envoy-Internal": "true"
        }
    }

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
* token validation (done)
* dropping of token header before forwarding message to Orion (done)
* response token header injection (in progress)
* K8s + Istio + adapter local and cloud deployment (done)
* mutual TLS (almost there!)
* Istio gateway / virtual service to handle IDS / Fiware message translation
  (will take blood, sweat and tears :-)
