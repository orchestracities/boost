package token

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io"
	"net"
	"net/http"
	"testing"
)

const accessToken = "you.fat.jwt"
const expiresIn = 3600
const serverAddr = ":44300"

// NOTE. mTLS setup.
// We use the localhost certificate (`testCert`) and associated private key
// (`testCertPvtKey`) in "dapsclient_test.go" for mTLS:
//
// * client cert: testCert
// * server cert & pvt key: testCert, testCertPvtKey
//
// So the client gets configured to present `testCert` (client's cert) to
// authenticate with the server and verifies server identity using the same
// `testCert` (server's cert). Likewise the server presents `testCert`
// (server's cert) to authenticate with the client and verifies client
// identity using `testCert` (client's cert). Can it get more confusing than
// this? I know, I'm a lazy bastard, I should've used different certs for
// client and server...

func tokenHandler(w http.ResponseWriter, r *http.Request) {
	data := fmt.Sprintf(`{"access_token": "%s", "expires_in": %d}`,
		accessToken, expiresIn)
	io.WriteString(w, data)
}

func serverTLSConfig() (*tls.Config, error) {
	// client cert
	caCertPool := x509.NewCertPool()
	if !caCertPool.AppendCertsFromPEM([]byte(testCert)) {
		return nil, fmt.Errorf("can't add client cert to server pool")
	}

	// server cert - same as client's, out of convenience
	srvCert, err := tls.X509KeyPair([]byte(testCert), []byte(testCertPvtKey))
	if err != nil {
		return nil, err
	}

	tlsConfig := &tls.Config{
		ClientCAs:    caCertPool,
		ClientAuth:   tls.RequireAndVerifyClientCert,
		Certificates: []tls.Certificate{srvCert},
	}
	tlsConfig.BuildNameToCertificate()

	return tlsConfig, nil
}

func enterServerLoop(srv *http.Server, ctl chan error) {
	config, err := serverTLSConfig()
	if err != nil {
		ctl <- err
		return
	}

	tcpSocket, err := net.Listen("tcp", srv.Addr)
	if err != nil {
		ctl <- err
		return
	}
	tlsSocket := tls.NewListener(tcpSocket, config)
	if err != nil {
		ctl <- err
		return
	}

	defer tcpSocket.Close()
	ctl <- srv.Serve(tlsSocket)
}

func startServer() (*http.Server, chan error) {
	http.HandleFunc("/token", tokenHandler)

	ctl := make(chan error)
	server := &http.Server{Addr: serverAddr}
	go enterServerLoop(server, ctl)

	return server, ctl
}

func TestCanRetrieveToken(t *testing.T) {
	daps := &DapsIDRequest{
		connectorID:          "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
		connectorAudience:    "https://consumerconnector.fiware.org",
		secondsBeforeExpiry:  3600,
		privateKey:           testCertPvtKey,
		connectorCertificate: testCert,
		serverCertificate:    testCert,
		serverHost:           "localhost" + serverAddr,
	}

	server, errCh := startServer()
	got, clientErr := daps.IdentityToken()
	shutdownErr := server.Shutdown(context.TODO())
	svrErr := <-errCh
	if clientErr != nil {
		msg := "can't get token; errors:\nclient: %v\nshutdown: %v\nserver: %v"
		t.Errorf(msg, clientErr, shutdownErr, svrErr)
	}
	if got != accessToken {
		t.Errorf("want: %s; got: %s", accessToken, got)
	}
}
