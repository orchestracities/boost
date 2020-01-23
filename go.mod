module github.com/orchestracities/boost

go 1.13

require (
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/gogo/protobuf v1.3.0
	github.com/google/uuid v1.1.1
	google.golang.org/grpc v1.24.0
	istio.io/api v0.0.0-20191115171246-7a8183d7e4bf
	istio.io/istio v0.0.0-20191120065046-31a840407eca
	istio.io/pkg v0.0.0-20191113122952-4f521de9c8ca
)

// NOTE. Dependency hell.
// We depend on Istio (master at 20 Nov 2019) which in turn depends on K8s.
// Those K8s libs don't mesh well w/ the new Go module system and so we're
// forced to C&P the lines below **verbatim** from the Istio `go.mod` file.
// Keep this in mind as you update dependencies, it's an accident waiting
// to happen!

// Kubernetes makes it challenging to depend on their libraries. To get around this, we need to force
// the sha to use. All of these are pinned to the tag "kubernetes-1.16"
replace k8s.io/api => k8s.io/api v0.0.0-20191003000013-35e20aa79eb8

replace k8s.io/apimachinery => k8s.io/apimachinery v0.0.0-20190913080033-27d36303b655

replace k8s.io/cloud-provider => k8s.io/cloud-provider v0.0.0-20191003003426-b4b1f434fead

// Pinned to Kubernetes 1.15 for now, due to some issues with 1.16
// TODO(https://github.com/istio/istio/issues/17831) upgrade to 1.16
replace k8s.io/client-go => k8s.io/client-go v0.0.0-20191003000419-f68efa97b39e

replace k8s.io/apiserver => k8s.io/apiserver v0.0.0-20191003001037-3c8b233e046c

replace k8s.io/apiextensions-apiserver => k8s.io/apiextensions-apiserver v0.0.0-20191003002041-49e3d608220c

replace k8s.io/kubelet => k8s.io/kubelet v0.0.0-20191003002833-e367e4712542

replace k8s.io/cli-runtime => k8s.io/cli-runtime v0.0.0-20191003002408-6e42c232ac7d

replace k8s.io/kube-controller-manager => k8s.io/kube-controller-manager v0.0.0-20191003003129-09316795c0dd

replace k8s.io/code-generator => k8s.io/code-generator v0.0.0-20190927045949-f81bca4f5e85

replace k8s.io/kube-scheduler => k8s.io/kube-scheduler v0.0.0-20191003003001-314f0beee0a9

replace k8s.io/kube-proxy => k8s.io/kube-proxy v0.0.0-20191003002707-f6b7b0f55cc0

replace k8s.io/cri-api => k8s.io/cri-api v0.0.0-20190828162817-608eb1dad4ac

replace k8s.io/csi-translation-lib => k8s.io/csi-translation-lib v0.0.0-20191003003551-0eecdcdcc049

replace k8s.io/legacy-cloud-providers => k8s.io/legacy-cloud-providers v0.0.0-20191003003732-7d49cdad1c12

replace k8s.io/component-base => k8s.io/component-base v0.0.0-20191003000551-f573d376509c

replace k8s.io/cluster-bootstrap => k8s.io/cluster-bootstrap v0.0.0-20191003003255-c493acd9e2ff

replace k8s.io/metrics => k8s.io/metrics v0.0.0-20191003002233-837aead57baf

replace k8s.io/sample-apiserver => k8s.io/sample-apiserver v0.0.0-20191003001538-80f33ca02582

replace k8s.io/kube-aggregator => k8s.io/kube-aggregator v0.0.0-20191003001317-a019a9d85a86
