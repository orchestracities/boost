Deployment and Scalability
--------------------------
> TODO!

Talk about

* build process---manual workflow but scripts automate most steps
* Docker image, publishing process (Docker hub, no CI/CD)
* versioning
* Istio config
    * required routing and message manipulation rules---gateway to
      service, envoy to adapter, egress filter
    * required adapter custom resources---template and adapter as
      output by code generation pipeline
* Boost demo profile
    * config generation process---yamster, codegen scripts
    * orion sidecar shortcomings---see #36
* local deployment with Minikube
* K8s cluster deployment
* scaling horizontally---e.g. K8s service w/ multiple adapter processes
* cache consistency issues when scaling horizontally

