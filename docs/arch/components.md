System Decomposition
--------------------
> TODO!

Key points

* 2 subsystems:
  - adapter and msg routing/manipulation (mostly Istio config but an
    essential part of the solution)
  - adapter gRPC and HTTP interfaces
  - routing/manipulation expected by adapter
  - tech stack
* adapter components (responsibilities, interfaces, dependencies)
  - endpoint: external communication, server interfaces
  - handler: security workflow
  - authz: AuthZ client and XACML DSL
  - consumer: token representations & utils
  - daps: DAPS client, token representations & utils
  - jwt: payload representations and validation
  - cache: what it sounds like
* code generation
  - adapter artefacts (see [Mixer Adapter Code Generation][codegen])
  - Istio config ([yamster][yamster]); demo profile; generating multiple
    config sets (e.g. dev, prod)



[codegen]: ../dev/codegen.md
[yamster]: ../../yamster/README.md
