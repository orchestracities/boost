package daps

import (
	"crypto/tls"
	"fmt"
	"io"
	"log"
	"net/http"

	"github.com/orchestracities/boost/mockdaps/https"
)

func mTLSConfig(serverPvtKeyFile, serverCertFile,
	clientCertFile string) (*tls.Config, error) {
	m := &https.MutualTLSConfig{}
	err := m.FromFiles(serverPvtKeyFile, serverCertFile, clientCertFile)
	if err != nil {
		return nil, err
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
func Serve(port, serverPvtKeyFile, serverCertFile, clientCertFile string) {
	config, err := mTLSConfig(serverPvtKeyFile, serverCertFile, clientCertFile)
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
