Boost Software Architecture
---------------------------
> A technical map of the software.

This document describes (only) the **technical** aspects of the Boost
architecture through a set of interlocked architectural viewpoints.
As such, it is only aimed at developers who need to understand the
big picture before modifying the architecture or extending the code
with new functionality.


### Document status

**Draft**. Even though this document is a first draft and many sections
need to be expanded, the included material should be enough to gain
a good understanding of the Boost architecture.


### Prerequisites

We assume the reader is well versed in distributed systems and cloud
computing. Also assumed is general knowledge of identity and access
management systems as well as the rudiments of symmetric/asymmetric
cryptography and digital signatures. Moreover, the reader should be
familiar with the following technologies: HTTP, TLS, gRPC, OAuth 2,
JWT, XACML, Docker, Kubernetes, Istio (in particular the old Mixer
architecture), FiWare (Context Broker, AuthZ, KeyRock), International
Data Spaces (IDS basic concepts, DAPS).


### Table of contents

1. [Overview][overview]. The basic ideas are summarised here and then
   further developed in later sections.
2. [System requirements][requirements]. An account of functional
   requirements and system quality attributes.
3. [Information model][info-model]. What information the system handles
   and how it is represented and processed, with an emphasis on security.
4. [System decomposition][components]. Subsystems and components,
   modularity, code generation.
5. [Message passing mechanics][messaging]. Distributed communication
   protocols and synchronisation, message routing and manipulation.
6. [Caching][caching].
7. [Deployment and scalability][deployment].
8. Quality assurance.




[caching]: ./caching.md
[components]: ./components.md
[deployment]: ./deployment.md
[info-model]: ./info-model.md
[messaging]: ./messaging.md
[overview]: ./overview.md
[requirements]: ./requirements.md
