Requirements
------------
> System requirements and functionality viewpoint.

This section gives an account of key functional requirements (i.e.
functions that the system should perform) and desired service quality
attributes (i.e. constraints on system functions, also known as non-functional
requirements) which have a deep impact on solution design and shape
the architecture.


### System functions

As pointed out in the overview section, the Boost project is a stepping
stone towards interoperability between FiWare and IDS platforms. The
Boost architecture represents an initial attempt to bridge the gap
between the security infrastructure of the two platforms. Specifically,
Boost provides the means to secure HTTP communication between:

1. An IDS service requesting an operation on a resource managed by a
   FiWare service, e.g. an IDS consumer reading entity data held by
   Context Broker.
2. A FiWare service requesting an operation on a resource managed by
   an IDS service, e.g. Context Broker notifications.

Boost is required to orchestrates a security workflow whereby parties
can reliably identify each other and grant access to resources only
to authorised entities, as determined by both IDS and FiWare security
policies. In the case of a request from an IDS consumer to a FiWare
service provider (1), Boost expects the consumer to supply both

* IDS security data. An RSA-signed JWT, issued by DAPS, containing
  consumer claims and additional information about the issuer. 
* FiWare security data. An HS-signed JWT obtained from KeyRock and
  containing FiWare consumer roles.

Boost verifies the signature of the IDS JWT in order to ascertain the
consumer's identity and then checks both sets of claims (IDS and FiWare)
before granting access to the requested resource. Finally, it obtains
a service identity from DAPS (in the form of a JWT) and attaches it
to the FiWare service response so that the consumer too can verify the
provider's identity. In the case of a request from FiWare to IDS (2),
Boost obtains service identity and claims from DAPS and inserts them
into the request so that the receiving end can carry out a similar
security workflow.


### Quality attributes

Separation of concerns is a highly desirable quality of any design
catering for scenarios (1) and (2) just outlined. Ideally, development
and deployment of security infrastructure should be separate from that
of the services to be protected so that services remain agnostic about
how HTTP operations are secured. Another important aspect to consider
is latency: implementing the security workflow just described typically
entails communication among different processes possibly running in
separate networks which adds to service latency. Thus care should be
taken to avoid degrading service latency to unacceptable levels.
