package daps

import (
	"crypto/tls"
	"fmt"
	"io"
	"log"
	"net/http"

	"github.com/orchestracities/boost/mockdaps/https"
)

// NOTE. mTLS setup.
// We use the localhost certificate (`TestCert`) and associated private key
// (`TestCertPvtKey`) defined in "config.go" for mTLS:
//
// * client cert: TestCert
// * server cert & pvt key: TestCert, TestCertPvtKey
//
// So the client gets configured to present `TestCert` (client's cert) to
// authenticate with the server and verifies server identity using the same
// `TestCert` (server's cert). Likewise the server presents `TestCert`
// (server's cert) to authenticate with the client and verifies client
// identity using `TestCert` (client's cert). Can it get more confusing than
// this? I know, I'm a lazy bastard, I should've used different certs for
// client and server...

func mTLSConfig() (*tls.Config, error) {
	m := &https.MutualTLSConfig{
		ClientCerts:      []https.PemData{TestCert},
		ServerCert:       TestCert,
		ServerCertPvtKey: TestCertPvtKey,
	}
	return m.ServerTLSConfig()
}

func tokenHandler(w http.ResponseWriter, r *http.Request) {
	log.Printf("REQUEST: %v", r)
	r.ParseForm()
	log.Printf("FORM DATA: %v", r.PostForm)

	data := fmt.Sprintf(`{"access_token": "%s", "expires_in": %d}`,
		AccessToken, ExpiresIn)
	io.WriteString(w, data)
}

func registerHandlers() {
	http.HandleFunc("/token", tokenHandler)
}

// Serve starts the DAPS mock server.
func Serve(port string) {
	config, err := mTLSConfig()
	if err != nil {
		log.Fatal(err)
		return
	}

	server := &http.Server{Addr: fmt.Sprintf(":%s", port)}
	log.Printf("daps mock server is going to be listening on %s", server.Addr)

	registerHandlers()
	feedback := https.Run(server, config)

	err = <-feedback
	log.Fatal(err)
}
