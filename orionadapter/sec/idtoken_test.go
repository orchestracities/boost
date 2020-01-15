package token

import (
	"github.com/dgrijalva/jwt-go"
	"math"
	"testing"
	"time"
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
		secondsBeforeExpiry: 10,
		privateKey:          privateKey,
	}
	requestJWT, err := r.requestToken()
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	if err := Validate(pubKey, requestJWT); err != nil {
		t.Errorf("should've built a valid JWT: %v", err)
	}
}

func TestCantBuildRequestTokenWithoutPvtKey(t *testing.T) {
	r := &DapsIDRequest{
		secondsBeforeExpiry: 10,
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
	{secondsBeforeExpiry: 0}, {secondsBeforeExpiry: 10},
	{secondsBeforeExpiry: 20}, {secondsBeforeExpiry: 3600},
}

func TestRequestTokenExpClaim(t *testing.T) {
	for k, r := range requestTokenExpClaim {
		r.privateKey = privateKey
		claims, err := buildAndDecodeRequestTokenPayload(&r)
		if err != nil {
			t.Errorf("[%d] should've built a JWT: %v", k, err)
		}
		now := time.Now().Unix()
		assertClose(t, now+int64(r.secondsBeforeExpiry), claims.ExpiresAt)
	}
}

func TestRequestTokenIssClaim(t *testing.T) {
	r := &DapsIDRequest{privateKey: privateKey}
	claims, err := buildAndDecodeRequestTokenPayload(r)
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	now := time.Now().Unix()
	assertClose(t, now, claims.IssuedAt)
}

func TestRequestTokenNbfClaim(t *testing.T) {
	r := &DapsIDRequest{privateKey: privateKey}
	claims, err := buildAndDecodeRequestTokenPayload(r)
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	now := time.Now().Unix()
	assertClose(t, now, claims.NotBefore)
}

var requestTokenIssAndSubClaims = []DapsIDRequest{
	{}, {connectorID: ""}, {connectorID: " "}, {connectorID: "x"},
	{connectorID: "2d80dc4e-7dfe-449c-8e3a-ce19b41685c3"},
}

func TestRequestTokenIssAndSubClaims(t *testing.T) {
	for k, r := range requestTokenIssAndSubClaims {
		r.privateKey = privateKey
		claims, err := buildAndDecodeRequestTokenPayload(&r)
		if err != nil {
			t.Errorf("[%d] should've built a JWT: %v", k, err)
		}
		if r.connectorID != claims.Issuer {
			t.Errorf("[%d] want iss=%s; got: %s",
				k, r.connectorID, claims.Issuer)
		}
		if r.connectorID != claims.Subject {
			t.Errorf("[%d] want sub=%s; got: %s",
				k, r.connectorID, claims.Subject)
		}
	}
}

var requestTokenAudClaim = []DapsIDRequest{
	{}, {connectorAudience: ""}, {connectorAudience: " "},
	{connectorAudience: "x"},
	{connectorAudience: "https://consumerconnector.fiware.org"},
}

func TestRequestTokenAudClaim(t *testing.T) {
	for k, r := range requestTokenAudClaim {
		r.privateKey = privateKey
		claims, err := buildAndDecodeRequestTokenPayload(&r)
		if err != nil {
			t.Errorf("[%d] should've built a JWT: %v", k, err)
		}
		if r.connectorAudience != claims.Audience {
			t.Errorf("[%d] want iss=%s; got: %s",
				k, r.connectorAudience, claims.Audience)
		}
	}
}
