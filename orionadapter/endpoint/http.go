package endpoint

// TODO: stop gap solution to https://github.com/orchestracities/boost/issues/24
// Sort out egress routing, then ditch this code and the Envoy filter.

import (
	"fmt"
	"net/http"

	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
	"github.com/orchestracities/boost/orionadapter/handler"
)

var currentAdapterConfig *config.Params = nil

// TODO: concurrency!! currentAdapterConfig gets shared between gRPC
// and HTTP servers!!
// TODO: currentAdapterConfig will be nil until the first incoming
// request for Orion hits the mesh---see server.HandleOrionadapter.
// Hence any notification Orion will send before then, will have
// no DAPS server token!!

func token(w http.ResponseWriter, req *http.Request) {
	logRequest(req)

	if currentAdapterConfig == nil {
		msg := "http endpoint hasn't been initialized yet"
		ilog.Errorf("%s", msg)
		http.Error(w, msg, http.StatusInternalServerError)
		return
	}

	serverToken, err := handler.GenerateToken(currentAdapterConfig)
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
