Overview
--------
> The why, the what and the how.

This introductory section first touches on project motivation and
goals, then goes on to sketching the architecture conceptual model
and how it has been implemented through an Istio mesh.


### Motivation and goals

FiWare is a well established, open source IoT platform whose goals
include breaking information silos and enabling the data economy.
IDS is a more recent initiative with similar goals, whose stated
mission is to foster data exchange and sovereignty. Given the large
communities and dozens of industrial partners that the two projects
have attracted, it is not hard to see that having FiWare inter-operate
with IDS could spark new and exciting opportunities in the data economy.

The Boost project is an initial attempt to bridge the gap between
the two platforms by providing the means to secure HTTP communication
between IDS and FiWare services in such a way that parties can reliably
identify each other (through IDS security infrastructure) and grant
access to resources only to authorised entities, as determined by
both IDS and FiWare security policies. Separation of concerns is a
key project goal: development and deployment of security infrastructure
has to be separate from that of the services to be protected so that
services remain agnostic about how HTTP operations are secured.


### Concept

An HTTP message interception framework to supplement HTTP services
with IDS and FiWare security. HTTP traffic selection rules, defined
within the framework independently of HTTP services, determine which
incoming and outgoing messages should be subject to security screening
before reaching their intended service destination endpoint. Any
incoming, matching request is initially re-routed to a separate
process, external and independent of the target service, that is in
charge of orchestrating client authentication and authorisation with
configured DAPS (IDS security) and AuthZ (FiWare) servers, using any
credentials found in the message. If the given credentials fail some
of the security checks, the request is discarded (i.e. does not reach
the target service) and a suitable response (e.g. HTTP `403`) is sent
back to the client. Otherwise, if all checks succeed, the request is
routed once more, this time to the target service. Then the service
response is also intercepted to inject DAPS credentials which identify
the target service and which the security orchestration process obtained
from the configured DAPS server on behalf of the service. Similar to
incoming traffic, if a matching HTTP request originates from one of
the framework-monitored services, the corresponding DAPS identification
and access control credentials are transparently added to the request
before forwarding it to its intended destination. The following illustration
depicts the processing of incoming traffic in the case the client
supplies suitable credentials to access the requested resource.

![Conceptual model of incoming traffic processing.][concept.dia]

We see how the client request is intercepted by a message routing
and manipulation component (1) which passes client credentials on
to the orchestration component (2). After successfully checking the
credentials (3), which includes verifying the client's identity, the
orchestration component requests identity credentials for the target
service from DAPS (4) and then asks AuthZ (5) whether the client is
authorised to perform the requested operation, given the supplied
credentials. On receiving AuthZ's approval, orchestration forwards
service identity data to the routing and manipulation component so
that it can later be added to the service response (6). Then the
original request, stripped of credentials, is routed to the target
service (7) and finally, the service response is sent back to the
client, after inserting service identity data (8).


### Implementation

Having defined the abstract ideas, we are now ready to explain how
they have been realised but, in order to understand what follows,
readers should have a good command of Istio's [Mixer architecture][istio.mix]
and its [configuration model][istio.mix-cfg].

In a nutshell, Boost is an Istio service mesh with a custom Mixer
adapter which orchestrates the security workflow just described. In
somewhat more detail, services are deployed (through Docker images)
to an Istio-managed Kubernetes cluster configured to allow the custom
adapter to examine HTTP service traffic flow and possibly request to
alter it. Thus, the adapter plays the role of the orchestrator in our
conceptual model whereas Istio data and control planes (in particular
the Mixer and the Envoy proxies) provide message routing and manipulation.

The following UML communication diagram exemplifies incoming request
processing with a client request to retrieve a list of top level
resources from a FiWare Context Broker service. The client request
is initially intercepted by an Envoy process, masquerading as the
actual target service (`orion`), which extracts request data in the
form of Mixer attributes and passes them on to the Mixer process. In
turn, the Mixer forwards the extracted data to the adapter (`orad`,
short for Orion adapter) in order to know how the client request should
subsequently be handled. The adapter carries out various security
workflow tasks as detailed earlier (not shown in the diagram) and
determines the client is allowed to retrieve the resource at `/v2/`.
Accordingly, it returns a success flag to the Mixer along with `orion`'s
identity data obtained from DAPS. The Mixer relays the information
to the Envoy which, on seeing the green light, forwards the client
request to the actual service, `orion`. (Had the adapter returned a
failure flag, the Envoy would have sent a permission-denied response
to the client instead of forwarding the request.) Next, the Envoy
collects the service response, adds `orion`'s identity and returns
the augmented response to the client.

![Incoming request processing.][request.dia]

Although subsequent sections will provide greater detail about these
components, their interactions and other aspects of the architecture,
it is worth previewing a few points here. First, Istio traffic management
is a fundamental component of our solution. In fact, the above example
scenario suggests that message routing and manipulation rules ultimately
make it possible for the adapter to receive client security data extracted
from the example HTTP request header (`sec`) and for the Envoy to add
a new header (called `sec` too for the sake of argument) to the service
response, holding the DAPS identity retrieved by the adapter. Secondly,
cryptography techniques such as digital signatures and certificates
are used throughout to ascertain the identities of communicating parties
and ensure that credentials and other security claims are not tampered
with. Lastly, the adapter aggressively caches DAPS identities and AuthZ
authorisation decisions. This is an essential architectural feature
as Mixer built-in attribute caching alone is not enough to achieve
acceptable latency levels.


### Rationale

What is the rationale behind our design decisions? A few explanatory
words are in order. We have already mentioned that Boost aims to secure
HTTP services transparently, without requiring any service modification.
After exploring several avenues, it became clear that intercepting
and manipulating HTTP messages would fit best our requirements, especially
when considering scenarios involving Context Broker notifications
where the service to be protected initiates a connection to a remote
server. Service deployment was another factor that affected architectural
decisions: Kubernetes clusters are a popular choice to run FiWare
services. The Istio mesh killed two birds with one stone: Istio has
robust support for traffic management (which makes it possible to
configure HTTP routes and manipulate messages) on the one hand and
integrates tightly with Kubernetes on the other.




[concept.dia]: ./overview.concept.png
[istio.mix]: https://archive.istio.io/v1.4/docs/ops/deployment/architecture/
    "Istio - Mixer Architecture"
[istio.mix-cfg]: https://archive.istio.io/v1.4/docs/reference/config/policy-and-telemetry/mixer-overview/
    "Istio - Mixer Configuration Model"
[request.dia]: ./overview.incoming-request.png
