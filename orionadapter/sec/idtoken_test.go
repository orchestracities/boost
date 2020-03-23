package token

import (
	"fmt"
	"math"
	"testing"
	"time"

	"github.com/dgrijalva/jwt-go"
)

// NOTE. We'll use pvt/pub key pair declared in "tokenval_test.go".

func buildAndDecodeRequestTokenPayload(r *DapsIDRequest) (*jwt.StandardClaims, error) {
	requestJWT, err := r.requestToken()
	if err != nil {
		return &jwt.StandardClaims{}, err
	}
	claims := &jwt.StandardClaims{}
	_, err = jwt.ParseWithClaims(requestJWT, claims,
		func(t *jwt.Token) (interface{}, error) {
			return toRsaPubKey(pubKey)
		})
	if err != nil {
		return &jwt.StandardClaims{}, err
	}
	return claims, nil
}

func TestCanBuildValidRequestToken(t *testing.T) {
	r := &DapsIDRequest{
		SecondsBeforeExpiry: 10,
		PrivateKey:          privateKey,
	}
	requestJWT, err := r.requestToken()
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	if _, err := Validate(pubKey, requestJWT); err != nil {
		t.Errorf("should've built a valid JWT: %v", err)
	}
}

func TestCantBuildRequestTokenWithoutPvtKey(t *testing.T) {
	r := &DapsIDRequest{
		SecondsBeforeExpiry: 10,
	}
	_, err := r.requestToken()
	if err == nil {
		t.Errorf("shouldn't have built a JWT: %v", err)
	}
}

func dist(t int64, u int64) float64 {
	diff := float64(t - u)
	return math.Abs(diff)
}

func assertClose(t *testing.T, a int64, b int64) {
	d := dist(a, b)
	if d > 2 {
		t.Errorf("values are too far apart: a = %d, b = %d", a, b)
	}
}

var requestTokenExpClaim = []DapsIDRequest{
	{SecondsBeforeExpiry: 0}, {SecondsBeforeExpiry: 10},
	{SecondsBeforeExpiry: 20}, {SecondsBeforeExpiry: 3600},
}

func TestRequestTokenExpClaim(t *testing.T) {
	for k, r := range requestTokenExpClaim {
		r.PrivateKey = privateKey
		claims, err := buildAndDecodeRequestTokenPayload(&r)
		if err != nil {
			t.Errorf("[%d] should've built a JWT: %v", k, err)
		}
		now := time.Now().Unix()
		assertClose(t, now+int64(r.SecondsBeforeExpiry), claims.ExpiresAt)
	}
}

func TestRequestTokenIssClaim(t *testing.T) {
	r := &DapsIDRequest{PrivateKey: privateKey}
	claims, err := buildAndDecodeRequestTokenPayload(r)
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	now := time.Now().Unix()
	assertClose(t, now, claims.IssuedAt)
}

func TestRequestTokenNbfClaim(t *testing.T) {
	r := &DapsIDRequest{PrivateKey: privateKey}
	claims, err := buildAndDecodeRequestTokenPayload(r)
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	now := time.Now().Unix()
	assertClose(t, now, claims.NotBefore)
}

var requestTokenIssAndSubClaims = []DapsIDRequest{
	{}, {ConnectorID: ""}, {ConnectorID: " "}, {ConnectorID: "x"},
	{ConnectorID: "2d80dc4e-7dfe-449c-8e3a-ce19b41685c3"},
}

func TestRequestTokenIssAndSubClaims(t *testing.T) {
	for k, r := range requestTokenIssAndSubClaims {
		r.PrivateKey = privateKey
		claims, err := buildAndDecodeRequestTokenPayload(&r)
		if err != nil {
			t.Errorf("[%d] should've built a JWT: %v", k, err)
		}
		if r.ConnectorID != claims.Issuer {
			t.Errorf("[%d] want iss=%s; got: %s",
				k, r.ConnectorID, claims.Issuer)
		}
		if r.ConnectorID != claims.Subject {
			t.Errorf("[%d] want sub=%s; got: %s",
				k, r.ConnectorID, claims.Subject)
		}
	}
}

var requestTokenAudClaim = []DapsIDRequest{
	{}, {ConnectorAudience: ""}, {ConnectorAudience: " "},
	{ConnectorAudience: "x"},
	{ConnectorAudience: "https://consumerconnector.fiware.org"},
}

func TestRequestTokenAudClaim(t *testing.T) {
	for k, r := range requestTokenAudClaim {
		r.PrivateKey = privateKey
		claims, err := buildAndDecodeRequestTokenPayload(&r)
		if err != nil {
			t.Errorf("[%d] should've built a JWT: %v", k, err)
		}
		if r.ConnectorAudience != claims.Audience {
			t.Errorf("[%d] want iss=%s; got: %s",
				k, r.ConnectorAudience, claims.Audience)
		}
	}
}

func TestIdentityTokenErrorWhenCantBuildClient(t *testing.T) {
	r := &DapsIDRequest{}
	token, err := r.IdentityToken()
	if err == nil {
		t.Errorf("shouldn't have discarded unusable DAPS client: %s", token)
	}
}

func TestRequestTokenErrorBuilding(t *testing.T) {
	cause := fmt.Errorf("whoa")
	if err := requestTokenError(cause); err == nil {
		t.Errorf("should've build an error")
	}
}
