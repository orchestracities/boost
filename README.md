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

You should see on screen a warning message asking you to edit
`_output_/testdata/sample_operator_cfg.yaml` to replace adapter
certs. Do it :-)

**Note**. `make-mix.sh`. It only runs on MacOS and Linux but it shouldn't
be impossible to tweak it to make it work on other OSes too.

Start our custom adapter

    $ go run orionadapter/main.go 43210 54321

With the adapter running and waiting for the Mixer server to hook up,
bring up the Mixer server in a new terminal:

    $ sh scripts/run-mixer.sh

Next up is our own test DAPS service. Open up a new term and:

    $ sh scripts/run-mockdaps.sh

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

At the moment, as a stopgap solution to [#24](https://github.com/orchestracities/boost/issues/24),
the adapter lets you get a DAPS ID token with a plain, old HTTP GET.
Try this:

    $ curl http://localhost:54321

The response body should be the Base64-encoded JSON object the adapter
got from the mock DAPS service. Have a look at the output on the adapter
and DAPS terminals to see what's going on under the bonnet. When you're
done, you may want to kill (`CTRL-c`) the adapter, mixer, and DAPS
processes and then close all your terminal windows. In fact, after
this sneak peek, we're ready for the main act.


### Local cluster deployment

Now on to something even more adventurous. We're going to run a Kubernetes
local cluster using Minikube, deploy Istio with its demo profile, and run
our adapter in it. Brace! Brace!

##### Deploying Istio

After installing Minikube, download the Istio release and install the demo
profile. Here's the short version, assuming you've already installed Minikube:

    $ cd ~

    # Start Minikube.

    $ minikube start --memory=16384 --cpus=4
    # Try --memory=4096 if you don't have that much RAM, it worked for us :-)
    $ kubectl config use-context minikube

    # Download and install Istio 1.4.2.

    $ export ISTIO_VERSION=1.4.2
    $ curl -L https://istio.io/downloadIstio | sh -
    $ cd istio-*
    $ export PATH="${PWD}/bin:${PATH}"
    # ...ideally you should add the above to your Bash profile.
    $ istioctl manifest apply --set profile=demo
    $ kubectl -n istio-system edit cm istio
    # ...set disablePolicyChecks to false ("i" for insert mode, "ESC" ":wq" for save & exit)
    $ kubectl label namespace default istio-injection=enabled

Long version:

- https://istio.io/docs/setup/getting-started/
- https://istio.io/docs/setup/additional-setup/sidecar-injection/#automatic-sidecar-injection
- https://istio.io/docs/tasks/policy-enforcement/enabling-policy/

**Note**. *Istio version*. Version `1.4.2` is the safest to use since
we've compiled and tested the adapter's gRPC interface against this
version. We also tested extensively with version `1.4.0` and `1.4.3`.
In principle what's documented in this README should work with any
`1.4.*` version and, barring minor adjustments, with `1.5.*` too.

**Note**. *Policy Enforcement*. The docs say the `demo` profile should enable
it (i.e. set `disablePolicyChecks` to `false`) but it doesn't nor does it
work to specify that option at installation time which is why you'll have
to manually edit the K8s config after applying the Istio `demo` profile.

**Tip**. *Istio Dashboard*. If you're looking for an easy way to see
what's going on in your mesh (services, logs, config, etc.), why not
use the Kiali dashboard installed with the demo profile? Try

    $ istioctl dashboard kiali

Log in with user `admin` and password `admin`.

##### Adapter and mock DAPS images

Let's "Dockerise" our adapter so we can run it on the freshly minted Istio
mesh. And as we're at it, we'll also build a Docker image for the mock
DAPS service we used earlier. We've got a script, `make-images.sh`, to
build them images and a nifty bit of sleight of hand will make Minikube
use them. In a new terminal:

    $ cd $GOPATH/src/orchestracities/boost/
    $ eval $(minikube docker-env)
    $ sh scripts/make-images.sh
    $ exit

The above basically stashes away our images in Minikube's own local Docker
registry so they can be fetched from there instead of trekking all
the way to DockerHub. This article explains how the trick works:

- https://dzone.com/articles/running-local-docker-images-in-kubernetes-1

##### Deploying dummy services

Our adapter is supposed to process attributes of messages directed to
Orion. For the sake of testing though, there's no need to deploy Orion
& friends, any dummy HTTP service will do but a better option is `httpbin`
which can echo back HTTP messages too---this will come in handy to e.g.
check the adapter removes IDS token headers before the HTTP request gets
passed down to the target service.

    $ cd $GOPATH/src/orchestracities/boost/
    $ kubectl apply -f deployment/httpbin_service.yaml
    $ kubectl apply -f deployment/ingress_routing.yaml

Another thing the adapter is supposed to do is getting an IDS identity
token from a DAPS service. Again, since we're just testing locally here,
we can get away with deploying our mock DAPS we dockerised earlier:

    $ kubectl apply -f deployment/mock_daps_service.yaml

Wait a bit until `httpbin`, `mockdaps` and all Istio services/pods are
alive & kicking. (If you don't have a beefy box, this will take a while,
like even 5 mins, go for coffee!)
Then you should be able to see what HTTP headers the `httpbin` service
gets to see when you `curl` a request:

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
    $ kubectl apply -f deployment/egress_filter.yaml
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

    PERMISSION_DENIED:
    orionadapter-handler.handler.istio-system:
    unauthorized: invalid consumer JWT data

Like I said earlier, the adapter verifies the JWT you send as part of the IDSA-Header is valid---see
`deployment/sample_operator_cfg.yaml`. What happens if we send a valid
token then? Here's a valid JWT signed with the private key in the config
(`idsa_private_key` field).

    $ export MY_FAT_JWT=eyJhbGciOiJSUzI1NiJ9.e30.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw

Now we can use this convenience script to stick it into a fully-fledged base64-encoded IDSA header:

    $ export HEADER_VALUE=$(sh scripts/idsa-header-value.sh "${MY_FAT_JWT}")

and send the header with

    $ curl -v "$BASE_URL"/headers \
        -H "header:${HEADER_VALUE}"

The request should go through to the target `httpbin` service
which should reply with the HTTP headers it gets to see.
You should be able to
spot a `header` among the response headers: this is where we plonk in
the IDS server token we generate. (No seriously, no pun intended, the
response header we output, just like the request header, is also called
`header` :-)
What you see on your terminal should be similar to:

    HTTP/1.1 200 OK
    ...
    header: eyJAdHlwZSI6ImlkczpSZXN1bHRNZXNz...(adapter generated)...

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

Ideally, you shouldn't see the IDSA client header being echoed back
by `httpbin` (`headers` JOSN object), yet there it is in all its glory.
In fact, our routing was supposed to chop that head(-er) off before
sending the request on to `httpbin` which ain't happening at the
moment---see #11 about it.

What goes in the adapter's output `header` is a Base64-encoded JSON object
that actually holds the identity token the adapter got back from DAPS.
Since our adapter got configured to talk to the dodgy DAPS server we
deployed earlier, if you decode the header value you should get a JSON
object similar to:

    {
        "@type": "ids:ResultMessage",
        "id": "http://industrialdataspace.org/resultMessage/1a421b8c-3407-44a8-aeb9-253f145c869a",
        "issued": "2020-01-20T11:19:47Z",
        "modelVersion": "2.1.0",
        "issuerConnector": "https://companyA.com/connector/59a68243-dd96-4c8d-88a9-0f0e03e13b1b",
        "securityToken": {
            "@type": "ids:DynamicAttributeToken",
            "tokenFormat": "https://w3id.org/idsa/code/tokenformat/JWT",
            "tokenValue": "whoopsie.dapsie.jwt"
        }
    }

Actually, except for the `id` field (which will hold a different UUID
every time) and the `issued` field (current date/time) the JSON you
get back should be the same as the above.
In fact, the mock DAPS service always returns a hard-coded ID token,
i.e. `whoopsie.dapsie.jwt`. Keen on some real DAPS action? Edit the
`daps` config in `deployment/sample_operator_cfg.yaml` to have our
adapter do mTLS with your DAPS server of choice, then

    $ kubectl apply -f deployment/sample_operator_cfg.yaml

and voila, real DAPS identity tokens will be handed to you on
a silver platter.

Up to this point we've talked about what happens when an HTTP request
from a client outside the mesh comes in. Besides processing incoming
data, Orion can also notify subscribers of state changes. When this
happens, the adapter should add a header to the HTTP request Orion
makes to notify subscribers. Again the HTTP header name is `header`
and its value, like for the response `header` earlier, is supposed
to be a Base64-encoded JSON object with a DAPS identity token. Well,
at least that's the theory. Shouldn't we check what happens in practice
though?
Since we deployed `httpbin` instead of Orion, we'll have to simulate
ourselves requests originating from Orion's pod but this isn't a
train smash. Just get a shell on that box

    $ kubectl exec -it \
        $(kubectl get pods -lapp=httpbin \
            -o jsonpath='{.items[0].metadata.name}') -- bash

and install `curl`

    # apt update -y && apt install curl

Now we're ready to make an HTTP call hitting a server outside the
mesh. We'll reach out to our old friend `httpbin`, this time using
the instance at `httpbin.org`:

    # curl -v http://httpbin.org/headers

The response should be similar to

    HTTP/1.1 200 OK
    ...
    {
        "headers": {
            "Accept": "*/*",
            "Content-Length": "0",
            "Header": "eyJAdHlwZSI6Imlkczp...(adapter generated)...",
            "Host": "httpbin.org",
            "User-Agent": "curl/7.58.0",
            ...
        }
    }

and if you decode the value of the JSON `Header` field from Base64,
you should get a JSON object like the one we saw earlier, with a
`tokenValue` of `whoopsie.dapsie.jwt` since the adapter got the
DAPS identity token from our mock DAPS service---well, unless you
reconfigured the mesh to have the adapter talk to a real DAPS, in
which case `tokenValue` should be a real DAPS identity token ;-)

Happy days!

##### Deploying Orion

Well, how about we do this with Orion instead of `httpbin`? Why the
heck not. Start by deploying MongoDB:

    $ kubectl apply -f deployment/mongodb_service.yaml

This is a simple MongoDB service with no replication and ephemeral
storage---i.e. your DB won't survive a pod restart---but will do
for testing. You should wait until MongoDB is up and running before
deploying Orion---in a prod scenario, you'd want to automate this
with e.g. `init` containers, but hey we're just testing here :-)
Instead of waiting around just twiddling your thumbs, edit your
load balancer config to add an external port for Orion:

    $ EDITOR=emacs kubectl -n istio-system edit svc istio-ingressgateway
    #        ^ replace with your fave or don't set the variable to use default

Then add the below port to the `ports` section:

    ports:
    ...
      - name: orion
        nodePort: 31026
        port: 1026
        protocol: TCP
        targetPort: 1026

This makes mesh gateway port `1026` reachable from outside the cluster
through port `31026`. Next deploy Orion

    $ kubectl apply -f deployment/orion_service.yaml

and you're ready to play around! Here's how to get your feet wet:

    $ source scripts/cluster-url.sh
    $ curl -v "$ORION_BASE_URL/v2"
    # you should get back a 403/permission denied.

    $ curl -v "$ORION_BASE_URL/v2" -H "header:${HEADER_VALUE}"
    # set HEADER_VALUE as we did earlier; you should get back some
    # JSON with Orion's API entry points.

You can try adding entities, subscriptions and trigger notifications.
It should all go without a hitch. Here's a smoke test.

    $ sh scripts/orion.post-entity.sh

creates a brand new Orion entity of type `Room` with an ID of `Room1`,
`pressure` and `temperature` attributes, whereas

    $ sh scripts/orion.sub.sh

tells Orion to notify our trustworthy friend at `httpbin.org` (we owe
you big time my china!) whenever that entity changes. To see it while
it's happening, start `tcpdump` in a separate terminal

    $ sudo tcpdump -i any -s 4096 -A host httpbin.org

then switch back to your current terminal and send a `Room1` update
with

    $ sh scripts/orion.update-entity.sh

Your `tcpdump` should've spewed out a giant cloud of text but if your
eyes can manage to separate the wheat from the chaff, you should be
able to catch the Orion notification coming out of the mesh towards
`httpbin.org`. It should look something like

    POST /post HTTP/1.1
    host: httpbin.org:80
    fiware-servicepath: /
    fiware-correlator: 449ec094-82fe-11ea-a8d6-0242ac110013
    ngsiv2-attrsformat: normalized
    header: eyJAdHlwZSI6ImlkczpSZXN1bH...(Orion's DAPS identity)...
    ...

    {"subscriptionId":"5e9d82045bfa0aeb50e8e21e",
     "data":[{
         "id":"Room1",
         "type":"Room",
         "temperature":{"type":"Float","value":21.5,"metadata":{}}}]}

Smoking can badly damage your health, so I won't encourage you to try
any more smoke tests but surely we've set the scene for your own,
hopefully smoke-free, tests.

##### Access-control with AuthZ

Time to up the ante in the access-control war. We're going to require
CIA clearance now before you can access HTTP resources---in case that
wasn't obvious to you too, CIA stands for Control of Internet Access,
of course, what else?! We have an AuthZ test server at

* http://authzforceingress.appstorecontainerns.46.17.108.63.xip.io/authzforce-ce/domains/CYYY_V2IEeqMJKbegCuurA/pdp

configured with an XACML policy that only lets users in roles `role0`
through `role3` `GET` Orion resources through an application identified
by a resource ID of `b3a4a7d2-ce61-471f-b05d-fb82452ae686`, i.e. our
Mr Adapter the Constable. Also, the policy only gives the green light
if the resource the user is trying to access belongs to the `service`
tenant.

With default config, the adapter won't ask AuthZ to authorize calls:
if the incoming token is valid, the request gets forwarded to Orion.
But you can change that in a flash. Edit `sample_operator_cfg.yaml`
to set the `authz/enable` flag to `true`, then

    $ kubectl apply -f deployment/sample_operator_cfg.yaml

Now whenever a request comes in, after okaying the client token in the
`header` header, the adapter will submit an authorization request to
AuthZ with the below data:

* *Resource ID*. Taken from `authz` config section.
* *Resource Path*. The incoming request path, e.g. `/v2/entities?id=1`.
* *Action*. Request verb, e.g. `GET`.
* *Tenant*. Content of the request's `Fiware-Service` header if any.
* *Roles*. User roles extracted from the `scopes` claim, if any, in
  the incoming JWT token payload.

Let's see it in action. Try resubmitting the request we made earlier
to get Orion's API entry points

    $ curl -v "$(minikube ip):31026/v2" -H "header:${HEADER_VALUE}"

and, surprise, surprise, the adapter should show you the door this
time (the boy ain't got no manners!) with a `403` and a message like

    PERMISSION_DENIED:
    orionadapter-handler.handler.istio-system:unauthorized:
    AuthZ denied authorization

Let's see if we can get through. Since AuthZ expects the user to be
in `role0` through `role3` and the request to target the `service`
tenant, we'll need to change our request so it holds that data too.
So we're going to use a JWT with a `scopes` claim set to `[role0, role1,
role2, role3]` and add a `Fiware-Service` header. Here's the JWT,
signed with the private key in the adapter config---look for the
`idsa_private_key` field in `sample_operator_cfg.yaml`.

    $ export MY_FAT_JWT=eyJhbGciOiJSUzI1NiJ9.eyJzY29wZXMiOlsicm9sZTAiLCJyb2xlMSIsInJvbGUyIiwicm9sZTMiXX0.JN66SWLPqNg7pqTFRcryo-3lX4V4BNKG5bZD3SDne4B3qV5kS-5NNW5wFkty870NFjuXP_nCxg3ayOCe8YZab3kRieaCeygVJwc2i1iUEHmYqKz6jx2EecfM2VbechaapDOFc9k01S5ea1t7fSHFsJsDWpVPpCJZBAv1ikPZrv88-7PLOacdGum--0-0gI6LGaXIFiTIAzbdeJ5V-ikIK7CgLJFaR3Ib5MwGRjrGTaPqQGE62SVpATphRhSJIfXm18ViF2fG7KTGPBYGY3rxAdy6l3klpKuxA0ATQRZJ39mpjrgbf-WVlvH_9nSFAn9BvLiSJohpSMmoJTX7ToWA0g

Just like we did earlier, we use the same convenience script to get
the base64-encoded IDSA header

    $ export HEADER_VALUE=$(sh scripts/idsa-header-value.sh "${MY_FAT_JWT}")

and we're ready to try our luck

    $ curl -v "$(minikube ip):31026/v2" \
           -H "header:${HEADER_VALUE}"  \
           -H "Fiware-Service:service"

If everything went according to plan, you're looking at a `200`
response on your terminal with the JSON body returned by Orion :-)

##### Caching

Oh my, saving the best for last. So for a [whole bunch of
reasons](https://github.com/orchestracities/boost/issues/9),
we wound up with our own caching solution instead of the Mixer's.
(Lucky us, fun times.) Fingers crossed, our caching should be decent
enough for most scenarios but time will tell. If you dig deep into the
adapter logs, you should be able to see that it caches calls to DAPS
and AuthZ. A DAPS ID token gets cached for the amount of time specified
in the JWT `exp` field. Likewise, an AuthZ decision gets cached until
the consumer JWT expires.

For example, if you look at the logs to see what happened while the
adapter serviced the last call we made to Orion, you should be able
to spot a message similar to

    AuthZ
      Request: &{
        Roles: [role0 role1 role2 role3]
        ResourceID: b3a4a7d2-ce61-471f-b05d-fb82452ae686
        ResourcePath: /v2
        FiwareService: service
        Action: GET
      }
      Decision: Permit
      Caching: decision not saved to cache

Uh, what's that "decision **not** saved to cache"? Why?! Well when
we whipped together that `MY_FAT_JWT` earlier we did it a bit too
quickly. Without an `exp` field, you can't expect calls to get
cached, can you? If besides the roles, you also add an `exp` field
with a Unix timestamp in the future, reexport `HEADER_VALUE` and
repeat the call, the log message should read

    AuthZ
      Request: ...
      Decision: Permit
      Caching: decision saved to cache

Ah, AuthZ's decision got cached. Try the same call again now and
then you should see a cache hit

    AuthZ
      Request: ...
      Decision: Permit (cached)

Just in case you're wondering. Caching takes into account all call
inputs so if you change the JWT (or HTTP method/path, etc.) just
slightly the adapter will ask again his old friend AuthZ for permission.


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
* dropping of token header before forwarding message to Orion (to do, see #11)
* response token header injection (done)
* K8s + Istio + adapter local and cloud deployment (done)
* mutual TLS (almost there!)
* Istio gateway / virtual service to handle IDS / Fiware message translation
  (will take blood, sweat and tears :-)
