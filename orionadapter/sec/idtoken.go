package token

import (
	"crypto/rsa"
	"fmt"
	"net/url"
	"time"

	"github.com/dgrijalva/jwt-go"
)

func toRsaPvtKey(pemRep string) (*rsa.PrivateKey, error) {
	keyBytes := []byte(pemRep)
	key, err := jwt.ParseRSAPrivateKeyFromPEM(keyBytes)
	if err != nil {
		return nil, invalidPvtKeyError(err)
	}
	return key, nil
}

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

func (r *DapsIDRequest) standardClaims() *jwt.StandardClaims {
	now := time.Now().Unix()
	return &jwt.StandardClaims{
		IssuedAt:  now,
		NotBefore: now,
		ExpiresAt: now + int64(r.SecondsBeforeExpiry),
		Subject:   r.ConnectorID,
		Issuer:    r.ConnectorID,
		Audience:  r.ConnectorAudience,
	}
}

// build JWT to use in request to get an ID token from DAPS.
func (r *DapsIDRequest) requestToken() (string, error) {
	key, err := toRsaPvtKey(r.PrivateKey)
	if err != nil {
		return "", err
	}
	claims := r.standardClaims()
	token := jwt.NewWithClaims(jwt.SigningMethodRS256, claims)
	return token.SignedString(key)
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

func invalidPvtKeyError(cause error) *jwt.ValidationError {
	msg := fmt.Sprintf("invalid private key: %v", cause)
	return jwt.NewValidationError(msg, jwt.ValidationErrorUnverifiable)
}

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
