package token

import (
	"crypto/rsa"
	"fmt"
	"net/url"
	"time"

	"github.com/dgrijalva/jwt-go"
)

func invalidPvtKeyError(cause error) *jwt.ValidationError {
	msg := fmt.Sprintf("invalid private key: %v", cause)
	return jwt.NewValidationError(msg, jwt.ValidationErrorUnverifiable)
}

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
	connectorID string
	// e.g. "https://consumerconnector.fiware.org"
	connectorAudience string
	// how many seconds from now before the JWT in the request expires.
	secondsBeforeExpiry uint
	// connector's own RSA private key in PEM format.
	privateKey string
	// connector's own certificate to authenticate with DAPS; paired to
	// the private key and in PEM format.
	connectorCertificate string
	// DAPS server certificate, in PEM format, the connector should use
	// to authenticate the server.
	serverCertificate string
	// DAPS server host or host:port.
	serverHost string
}

func (r *DapsIDRequest) standardClaims() *jwt.StandardClaims {
	now := time.Now().Unix()
	return &jwt.StandardClaims{
		IssuedAt:  now,
		NotBefore: now,
		ExpiresAt: now + int64(r.secondsBeforeExpiry),
		Subject:   r.connectorID,
		Issuer:    r.connectorID,
		Audience:  r.connectorAudience,
	}
}

// build JWT to use in request to get an ID token from DAPS.
func (r *DapsIDRequest) requestToken() (string, error) {
	key, err := toRsaPvtKey(r.privateKey)
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
	daps, err := NewDapsClient(r.serverHost, r.privateKey,
		r.connectorCertificate, r.serverCertificate)
	if err != nil {
		return "", err
	}

	reqToken, err := r.requestToken()
	if err != nil {
		return "", err
	}

	resData, err := daps.PostForm("/token", buildOauthRequest(reqToken), true)
	if err != nil {
		return "", err
	}

	resToken := &oauthTokenResponse{}
	err = resData.AsJSON(resToken)
	if err != nil {
		return "", err
	}

	if len(resToken.JwtData) == 0 {
		return "", fmt.Errorf("DAPS returned an empty token")
	}
	return resToken.JwtData, nil
}
