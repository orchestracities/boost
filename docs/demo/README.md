Whirlwind Tour
--------------
> A fast-paced tour of Boost features, hold on to your seat!

We're going to demo every Boost feature using progressively more complex
scenarios. Before you start make sure you got the Boost basics by reading
the [architecture overview][arch.overview]—you can skim through most
of it but eyeball the Concept and Implementation sections.

Ready, steady, go...

1. [Set up your dev env][dev-env].
2. [Take the adapter for a spin][spin].
3. [Local cluster deployment][local-cluster].
  1. [Deploy Istio][deploy-istio].
  2. [Build adapter and mock DAPS images][images].
  3. [Deploy dummy services and get to know the mesh][deploy-dummies].
  4. [Deploy & test the adapter][deploy-adapter].
  5. [Deploy & test Orion][deploy-orion].
  6. [Enforce access-control with AuthZ][authz].
  7. [Check out caching][caching].
  8. [Clean up][clean-up].
4. Rancher deployment. Coming soon!




[arch.overview]: ../arch/overview.md
[authz]: ./3.6.authz.md
[caching]: ./3.7.caching.md
[clean-up]: ./3.8.clean-up.md
[dev-env]: ./1.dev-env.md
[deploy-adapter]: ./3.4.deploy-adapter.md
[deploy-dummies]: ./3.3.deploy-dummies.md
[deploy-orion]: ./3.5.deploy-orion.md
[deploy-istio]: ./3.1.deploy-istio.md
[images]: ./3.2.images.md
[local-cluster]: ./3.local-cluster.md
[spin]: ./2.take-for-spin.md
