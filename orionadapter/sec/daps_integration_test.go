package token

import (
	"context"
	"crypto/tls"
	"fmt"
	"io"
	"net/http"
	"testing"

	"github.com/orchestracities/boost/mockdaps/https"
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

func mTLSConfig() (*tls.Config, error) {
	m := &https.MutualTLSConfig{
		ClientCerts:      []https.PemData{testCert},
		ServerCert:       testCert,
		ServerCertPvtKey: testCertPvtKey,
	}
	return m.ServerTLSConfig()
}

type MsgHandler func(w http.ResponseWriter, r *http.Request)
type Dispatcher struct {
	handle MsgHandler
}

func (d *Dispatcher) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	d.handle(w, r)
}

func startServer(d *Dispatcher) (*http.Server, chan error) {
	config, err := mTLSConfig()
	if err != nil {
		return nil, nil
	}
	server := &http.Server{
		Addr:    serverAddr,
		Handler: d,
	}
	return server, https.Run(server, config)
}

func doIDInteractionWith(h MsgHandler) (
	got string, clientErr, shutdownErr, svrErr error) {

	daps := &DapsIDRequest{
		ConnectorID:          "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
		ConnectorAudience:    "https://consumerconnector.fiware.org",
		SecondsBeforeExpiry:  3600,
		PrivateKey:           testCertPvtKey,
		ConnectorCertificate: testCert,
		ServerCertificate:    testCert,
		ServerHost:           "localhost" + serverAddr,
	}

	dispatcher := &Dispatcher{handle: h}
	server, errCh := startServer(dispatcher)
	got, clientErr = daps.IdentityToken()
	shutdownErr = server.Shutdown(context.TODO())
	svrErr = <-errCh

	return got, clientErr, shutdownErr, svrErr
}

func successTokenHandler(w http.ResponseWriter, r *http.Request) {
	data := fmt.Sprintf(`{"access_token": "%s", "expires_in": %d}`,
		accessToken, expiresIn)
	io.WriteString(w, data)
}

func TestCanRetrieveToken(t *testing.T) {
	got, clientErr, shutdownErr, svrErr :=
		doIDInteractionWith(successTokenHandler)
	if clientErr != nil {
		msg := "can't get token; errors:\nclient: %v\nshutdown: %v\nserver: %v"
		t.Errorf(msg, clientErr, shutdownErr, svrErr)
	}
	if got != accessToken {
		t.Errorf("want: %s; got: %s", accessToken, got)
	}
}

func emptyTokenHandler(w http.ResponseWriter, r *http.Request) {
	data := fmt.Sprintf(`{"access_token": "", "expires_in": %d}`, expiresIn)
	io.WriteString(w, data)
}

func TestErrorWhenReturnedTokenIsEmpty(t *testing.T) {
	got, clientErr, shutdownErr, svrErr :=
		doIDInteractionWith(emptyTokenHandler)
	if clientErr == nil {
		msg := "should reject empty token; errors:\nshutdown: %v\nserver: %v"
		t.Errorf(msg, shutdownErr, svrErr)
	}
	if got != "" {
		t.Errorf("want: ; got: %s", got)
	}
}

func missingTokenHandler(w http.ResponseWriter, r *http.Request) {
	data := fmt.Sprintf(`{"expires_in": %d}`, expiresIn)
	io.WriteString(w, data)
}

func TestErrorWhenReturnedTokenFieldIsMissing(t *testing.T) {
	got, clientErr, shutdownErr, svrErr :=
		doIDInteractionWith(missingTokenHandler)
	if clientErr == nil {
		msg := "should reject missing token; errors:\nshutdown: %v\nserver: %v"
		t.Errorf(msg, shutdownErr, svrErr)
	}
	if got != "" {
		t.Errorf("want: ; got: %s", got)
	}
}

func malformedTokenHandler(w http.ResponseWriter, r *http.Request) {
	io.WriteString(w, "{")
}

func TestTokenDeserializationError(t *testing.T) {
	got, clientErr, shutdownErr, svrErr :=
		doIDInteractionWith(malformedTokenHandler)
	if clientErr == nil {
		msg := "should reject invalid JSON; errors:\nshutdown: %v\nserver: %v"
		t.Errorf(msg, shutdownErr, svrErr)
	}
	if got != "" {
		t.Errorf("want: ; got: %s", got)
	}
}

func serverErrorHandler(w http.ResponseWriter, r *http.Request) {
	http.Error(w, "whoops!", http.StatusInternalServerError)
}

func TestServerError(t *testing.T) {
	got, clientErr, shutdownErr, svrErr :=
		doIDInteractionWith(serverErrorHandler)
	if clientErr == nil {
		msg := "should return server error; errors:\nshutdown: %v\nserver: %v"
		t.Errorf(msg, shutdownErr, svrErr)
	}
	if got != "" {
		t.Errorf("want: ; got: %s", got)
	}
}
