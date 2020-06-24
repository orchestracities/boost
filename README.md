Boost IDS/FiWare Connector
--------------------------
> Bridging the gap between IDS and FiWare.

Boost IDS/FiWare Connector is a distributed system to enable IDS and
FiWare services to securely exchange data. In somewhat more detail,
Boost is a mesh architecture to intercept incoming/outgoing HTTP messages
directed to/originating from mesh services and transparently carry out
a security workflow so that the functionality of existing HTTP services
can be augmented with IDS and FiWare security without requiring any
changes to the services themselves. The solution is based on the Istio
Mixer architecture whereby an out-of-process Mixer extension (adapter)
and Envoy filter collaborate to examine incoming/outgoing traffic and
perform security tasks.

This software has been developed by [Martel Innovate][martel] in collaboration
with the [FiWare Foundation][fiware] as part of the [Boost 4.0][boost4]
EU initiative.

**Note**. *Semantic overload*.
Within this repository's code and documentation, the term "Boost"
refers to the Boost IDS/FiWare Connector rather than the [Boost 4.0][boost4]
EU initiative. For example, the "Boost" in "Boost software architecture"
or "Boost mesh" should be understood as "Boost IDS/FiWare Connector".


### Documentation

The [overview][arch.overview] section of the [software architecture
document][arch] is the best starting point to learn about Boost: it
is a short read but presents the fundamental ideas clearly with the
help of diagrams. The reader interested in gaining a deeper understanding
of the architecture is invited to consider the entire [software architecture
document][arch]. A [hands-on tutorial][demo] demonstrates every Boost
feature through progressively more complex scenarios and exemplifies
mesh deployment and operations.


### Features at a glance

Currently the system can

* Intercept HTTP requests targeting any mesh service.
* Validate IDS header payload through configurable RSA infrastructure
  and deny access or forward requests to the target service according
  to validation outcome.
* Additionally turn on AuthZ access control so that past IDS header
  validation, the adapter also
    * validates KeyRock JWTs (through configurable shared secrets)
      and denies access if validation fails; otherwise
    * asks AuthZ to authorise the request and then denies access or
      forwards the request to the target service according to AuthZ's
      decision.
* Do mTLS with a configured DAPS server to get an ID token. (Keys and
  certificates are configurable too.)
* Insert the ID token into the service response, again using the IDS
  header mechanism, with a configurable JSON payload.
* Intercept HTTP requests originating from a mesh service and insert
  the DAPS ID token.
* Cache security operations and data to reduce service latency, with
  configurable expiry of AuthZ decisions.

A mesh demo configuration profile is provided too, featuring

* Orion, MongoDB and httpbin services—httpbin is very useful for testing.
* Mock DAPS server—again, useful for testing.
* Ingress/egress routing and telemetry.
* Routing of AuthZ requests to FiWare's Rancher instance.
* Mesh dashboards—Kiali & Grafana.

Note that the demo profile is not suitable for high-availability and/or
heavy-load scenarios as MongoDB and Orion are not replicated and data
are volatile—i.e. there is no persistent storage. However, as it stands,
the demo profile is an excellent starting point from which mesh administrators
can easily derive an Istio configuration to suit their specific needs.


### Project status

After several development cycles, including extensive testing and product
hardening, a beta implementation of the Boost architecture is now available
with all core features fully developed and ready for early adopters.




[arch]: ./docs/arch/README.md
[arch.overview]: ./docs/arch/overview.md
[boost4]: https://boost40.eu/
    "Big data for factories"
[demo]: ./docs/demo/README.md
[fiware]: https://www.fiware.org/foundation/
    "FiWare Foundation"
[martel]: https://www.martel-innovate.com/
    "Martel Innovate"
