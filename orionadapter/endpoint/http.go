package endpoint

// TODO: stop gap solution to https://github.com/orchestracities/boost/issues/24
// Sort out egress routing, then ditch this code and the Envoy filter.

import (
	"fmt"
	"net/http"

	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/cache"
	"github.com/orchestracities/boost/orionadapter/handler"
)

// TODO: currentAdapterConfig will be nil until the first incoming
// request for Orion hits the mesh---see grpc.HandleOrionadapter.
// Hence any notification Orion sends before then, will have no DAPS
// server token!!

func token(w http.ResponseWriter, req *http.Request) {
	logRequest(req)

	currentAdapterConfig, found := cache.LookupAdapterConfig()
	if !found {
		msg := "http endpoint hasn't been initialized yet"
		ilog.Errorf("%s", msg)
		http.Error(w, msg, http.StatusInternalServerError)
		return
	}

	serverToken, err := handler.GenerateProviderHeader(currentAdapterConfig)
	if err != nil {
		ilog.Errorf("error generating server token: %v", err)
		http.Error(w, "error generating server token",
			http.StatusInternalServerError)
		return
	}

	fmt.Fprintf(w, serverToken)
}

// RunHTTPServerLoop enters the HTTP server loop.
func RunHTTPServerLoop(port string) {
	http.HandleFunc("/", token)
	addr := fmt.Sprintf(":%s", port)

	http.ListenAndServe(addr, nil)
}

func logRequest(r *http.Request) {
	ilog.Infof("internal http token request: %s %v\n", r.RemoteAddr, r.Header)
}
