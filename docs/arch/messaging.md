Message Passing Mechanics
-------------------------
> How mesh processes communicate.

This section is devoted to distributed communication protocols and
synchronisation as well as message routing and manipulation. We begin
with a (custom) UML communication diagram illustrating how incoming
HTTP request data reach the adapter. The diagram shows various processes
communicating through different protocols to handle an incoming HTTP
request to retrieve a list of top level resources from Context Broker
and highlights the role that Istio (Kubernetes) custom resources play
in routing the request from the mesh gateway to Context Broker's Envoy
proxy, extracting data and ultimately forwarding them to the adapter
through the Mixer. The remainder of this section addresses each of
these aspects in more detail.

![Incoming request processing.][request.dia]


### Distributed communication

As explained in the previous sections, when a service is configured
with Boost security, additional processes will intervene in the handling
of incoming and outgoing HTTP traffic. When an HTTP request reaches the
mesh gateway, it is dispatched to a Kubernetes service where an Envoy
process intercepts it and extracts data from it in the form of Mixer
attributes that the Envoy then sends on to a Mixer process. Communication
between the Envoy and the Mixer happens over gRPC + TLS, with mutual
authentication. On receiving request attributes, the Mixer makes another
gRPC call to dispatch them to the adapter process. (Refer to the above
diagram for an example interaction among these processes.) Thus the
overall communication pattern is synchronous request-reply, but note
that Istio components make aggressive use of communication timeouts
which, depending on mesh load, may result in some requests being dropped
or responses being short-circuited. Also, note that in the case of
incoming HTTPS traffic, the gateway is typically configured to do
TLS termination.

Ideally HTTP requests originating from a mesh service would follow
the same communication patterns, albeit symmetrically. Unfortunately
this is not the case presently and a stopgap solution is in place to
insert DAPS identity tokens into requests. Briefly, this is done by
means of a filter attached to the Envoy process which intercepts any
outgoing service request, makes a direct HTTP call to the adapter to
obtain a DAPS token and then inserts it into the outgoing service
request. Take heed: this mechanism bypasses the Mixer but, to obtain
a DAPS token, the adapter needs to first access some configuration
values that the Mixer holds, therefore race conditions are possible,
although unlikely, whereby configuration items may be stale or not
available. Mechanics and shortcomings of this stopgap solution are
fully documented on GitHub, see [#25][gh-25] and [#24][gh-24].

Besides outgoing service traffic, there is also outgoing traffic
originating from the mesh control plane. In fact, in order to carry
out security workflow tasks, the adapter makes HTTPS calls to external
DAPS and AuthZ servers. Note that connections to DAPS require mutual
TLS authentication, thus the adapter initiates TLS connections accordingly.


### Incoming HTTP service traffic

In order to subject incoming requests and service responses to the
Boost security workflow, suitable message routing and manipulation
have to be implemented. We look at each aspect of the solution in
turn, starting from routing.

Routing of incoming request data happens on two levels: routing of
HTTP requests to Kubernetes services and routing of Mixer attributes
to the adapter. Both kinds of routing involve deploying Istio custom
resource descriptors (CRD) to the Istio system namespace (usually
called `istio-system`) in the Kubernetes cluster. Routing of an incoming
request to a service is typically done by means of a gateway and a
virtual service. The left hand side of the diagram at the beginning
of this section depicts a host-based routing scenario where a gateway
captures traffic directed to a `broker` host and a virtual service
routes it to an `orion` Kubernetes service. (This is a simple routing
scenario, but Istio traffic management is quite flexible and it is
possible to define more sophisticated routing rules, e.g. weighted
routing based on request headers and URL path.) For the request data
to continue its journey to the adapter, another route has to be in
place, namely the route from the Envoy proxy, through the Mixer, to
the adapter. This route is defined through rule and handler custom
resources. The right hand side of the diagram shows how a rule matching
traffic directed to the `orion` service acts as a precondition to
trigger a call to the `orad` adapter specified by the `handler` resource.

As noted earlier, if a route exists from the Envoy to the adapter,
on intercepting a matching HTTP request, the Envoy sends a collection
of attributes containing information about that request to the Mixer.
Nevertheless, how does the Mixer invoke the adapter? First of all,
since this is a gRPC call, the Mixer needs to know what is the server
interface that the adapter exposes. The adapter's gRPC interface is
defined by Protocol Buffers structures which are fed into a code generation
pipeline to produce server-side Go code (request/response structures,
marshalling, gRPC listener, etc.) to be included in the adapter and
Kubernetes custom resource descriptors to be deployed to the Istio
system namespace. (The process of code generation is rather tortuous,
see [this article][codegen] about it in the Boost developer resources.)
Although the gRPC interface definition becomes available to the Mixer
through these descriptors, before the Mixer can make a gRPC call to
send data along the configured adapter route, it still needs to know
what procedure to invoke (the adapter interface may define more than
one) and how to populate call input structures with attribute data.
To this end, another Istio custom resource, the instance, is used and
in fact for an adapter route to be well-formed, the rule descriptor
has to reference at least one instance. Presently, the Boost adapter
only exposes one remote procedure, thus, accordingly, only one instance
is required. Said instance has to reference the custom descriptor
containing the gRPC interface definition which was output by the code
generation pipeline. Moreover, the instance has to specify the Boost
message manipulation rules required for securing incoming HTTP traffic
which include request data extraction and insertion of a response header
containing the DAPS identity of the target service.

The right hand side of the diagram introduced earlier summarises this
rather lengthy discussion with an example. The rule descriptor references
an instance specifying a `template` of `ordata`. This is the name of
the gRPC interface descriptor output by the code generation pipeline
when fed with the (Istio) `Template` Protocol Buffers definition. The
two fields defined there (`url` and `jwt`) can then be used by the
instance to specify a map (`params`) from Mixer attributes to adapter
request fields: the request `path` attribute maps to `url` whereas the
`authz` header attribute maps to `jwt`. This way, on receiving a batch
of attributes, the Mixer can instantiate the `OrAdReq` input structure
(produced by the code generation pipeline) and populate it with the
values of the attributes selected by the instance descriptor as shown
in the gRPC call (3).


### Outgoing HTTP service traffic

As previously pointed out, in principle the same Istio routing and
message manipulation facilities discussed thus far should be able
to handle outgoing traffic, specifically the insertion of request
headers dynamically generated by the adapter. Unfortunately, this
does not seem to be the case (see GitHub [#24][gh-24]) and consequently
a stopgap solution had to be implemented (see GitHub [#25][gh-25])
to insert DAPS identity tokens into requests originating from a mesh
service configured with Boost security.

The solution involves attaching an Envoy HTTP filter to the sidecar
of each service requiring Boost security. The filter contains a Lua
script that retrieves, through an HTTP call, the DAPS identity from
the adapter and then inserts the corresponding Boost security header
into the outgoing service request.

The following YAML fragment highlights the key elements required to
attach a filter to the sidecar of the `orion` service in our running
example.

    ...
    kind: EnvoyFilter
    spec:
      configPatches:
      - applyTo: HTTP_FILTER
        match:
          context: SIDECAR_OUTBOUND
          listener:
            portNumber: 80
            ...
        patch:
          operation: INSERT_BEFORE
          value:
            config:
              inlineCode: "...Lua code to call adapter and insert header..."
            ...
      - applyTo: CLUSTER
        patch:
          operation: ADD
          value:
            hosts:
            - socket_address:
                address: "orad.istio-system"
                port_value: 54321
            ...
      workloadSelector:
        labels:
          app: orion

The workload selector block results in the filter being attached to
the sidecar of any Kubernetes service labelled `orion`. The filter
intercepts HTTP traffic through port `80` and has the Lua code process
it. For the Lua code to be able to call the adapter, its fully qualified
cluster name and port need to be specified, which is what the socket
address element is for.




[codegen]: ../dev/codegen.md
    "Mixer Adapter Code Generation"
[gh-24]: https://github.com/orchestracities/boost/issues/24
    "Dynamic HTTP header for outbound traffic"
[gh-25]: https://github.com/orchestracities/boost/pull/25
    "Stopgap solution to ID token for outbound requests"
[request.dia]: ./messaging.incoming-request.png
