package token

import (
	"fmt"
	"net/url"

	"github.com/orchestracities/boost/orionadapter/sec/jwt"
)

// DapsIDRequest holds the data needed to request an ID token from DAPS.
type DapsIDRequest struct {
	// identifies the connector within DAPS/IDS; usually a UUID.
	ConnectorID string
	// e.g. "https://consumerconnector.fiware.org"
	ConnectorAudience string
	// how many seconds from now before the JWT in the request expires.
	SecondsBeforeExpiry uint32
	// connector's own RSA private key in PEM format.
	PrivateKey string
	// connector's own certificate to authenticate with DAPS; paired to
	// the private key and in PEM format.
	ConnectorCertificate string
	// DAPS server certificate, in PEM format, the connector should use
	// to authenticate the server.
	ServerCertificate string
	// DAPS server host or host:port.
	ServerHost string
}

// build JWT to use in request to get an ID token from DAPS.
func (r *DapsIDRequest) requestToken() (string, error) {
	return jwt.MakeRS256SignedToken(
		r.PrivateKey, r.ConnectorID, r.ConnectorID,
		r.ConnectorAudience, r.SecondsBeforeExpiry)
}

func buildOauthRequest(requestToken string) url.Values {
	return url.Values{
		"grant_type":            {"client_credentials"},
		"client_assertion_type": {"urn:ietf:params:oauth:client-assertion-type:jwt-bearer"},
		"client_assertion":      {requestToken},
		"scope":                 {"ids_connector"},
	}
}

type oauthTokenResponse struct {
	JwtData   string `json:"access_token"`
	ExpiresIn int64  `json:"expires_in"`
}

// IdentityToken requests an ID token for the connector from DAPS.
func (r *DapsIDRequest) IdentityToken() (string, error) {
	daps, err := NewDapsClient(r.ServerHost, r.PrivateKey,
		r.ConnectorCertificate, r.ServerCertificate)
	if err != nil {
		return "", dapsClientInstantiationError(err)
	}

	reqToken, err := r.requestToken()
	if err != nil {
		return "", requestTokenError(err)
	}

	resData, err := daps.PostForm("/token", buildOauthRequest(reqToken), true)
	if err != nil {
		return "", dapsRequestError(err)
	}

	resToken := &oauthTokenResponse{}
	err = resData.AsJSON(resToken)
	if err != nil {
		return "", dapsDeserializationError(err)
	}

	if len(resToken.JwtData) == 0 {
		return "", dapsEmptyTokenError()
	}
	return resToken.JwtData, nil
}

// errors boilerplate

func dapsClientInstantiationError(cause error) error {
	return fmt.Errorf("can't instantiate DAPS client: %v", cause)
}

func requestTokenError(cause error) error {
	return fmt.Errorf("can't generate DAPS request token: %v", cause)
}

func dapsRequestError(cause error) error {
	return fmt.Errorf("DAPS request failed: %v", cause)
}

func dapsDeserializationError(cause error) error {
	return fmt.Errorf("can't parse DAPS response JSON: %v", cause)
}

func dapsEmptyTokenError() error {
	return fmt.Errorf("DAPS returned an empty token")
}
