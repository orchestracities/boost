package daps

import (
	"fmt"
	"math"
	"testing"
	"time"

	jot "github.com/dgrijalva/jwt-go"

	"github.com/orchestracities/boost/orionadapter/sec/jwt"
)

// RSA 256 keys generated on: https://jwt.io/

const pubKey = `-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSv
vkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHc
aT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIy
tvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0
e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWb
V6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9
MwIDAQAB
-----END PUBLIC KEY-----`

const privateKey = `-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWw
kWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mr
m/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEi
NQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV
3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2
QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQABAoIBACiARq2wkltjtcjs
kFvZ7w1JAORHbEufEO1Eu27zOIlqbgyAcAl7q+/1bip4Z/x1IVES84/yTaM8p0go
amMhvgry/mS8vNi1BN2SAZEnb/7xSxbflb70bX9RHLJqKnp5GZe2jexw+wyXlwaM
+bclUCrh9e1ltH7IvUrRrQnFJfh+is1fRon9Co9Li0GwoN0x0byrrngU8Ak3Y6D9
D8GjQA4Elm94ST3izJv8iCOLSDBmzsPsXfcCUZfmTfZ5DbUDMbMxRnSo3nQeoKGC
0Lj9FkWcfmLcpGlSXTO+Ww1L7EGq+PT3NtRae1FZPwjddQ1/4V905kyQFLamAA5Y
lSpE2wkCgYEAy1OPLQcZt4NQnQzPz2SBJqQN2P5u3vXl+zNVKP8w4eBv0vWuJJF+
hkGNnSxXQrTkvDOIUddSKOzHHgSg4nY6K02ecyT0PPm/UZvtRpWrnBjcEVtHEJNp
bU9pLD5iZ0J9sbzPU/LxPmuAP2Bs8JmTn6aFRspFrP7W0s1Nmk2jsm0CgYEAyH0X
+jpoqxj4efZfkUrg5GbSEhf+dZglf0tTOA5bVg8IYwtmNk/pniLG/zI7c+GlTc9B
BwfMr59EzBq/eFMI7+LgXaVUsM/sS4Ry+yeK6SJx/otIMWtDfqxsLD8CPMCRvecC
2Pip4uSgrl0MOebl9XKp57GoaUWRWRHqwV4Y6h8CgYAZhI4mh4qZtnhKjY4TKDjx
QYufXSdLAi9v3FxmvchDwOgn4L+PRVdMwDNms2bsL0m5uPn104EzM6w1vzz1zwKz
5pTpPI0OjgWN13Tq8+PKvm/4Ga2MjgOgPWQkslulO/oMcXbPwWC3hcRdr9tcQtn9
Imf9n2spL/6EDFId+Hp/7QKBgAqlWdiXsWckdE1Fn91/NGHsc8syKvjjk1onDcw0
NvVi5vcba9oGdElJX3e9mxqUKMrw7msJJv1MX8LWyMQC5L6YNYHDfbPF1q5L4i8j
8mRex97UVokJQRRA452V2vCO6S5ETgpnad36de3MUxHgCOX3qL382Qx9/THVmbma
3YfRAoGAUxL/Eu5yvMK8SAt/dJK6FedngcM3JEFNplmtLYVLWhkIlNRGDwkg3I5K
y18Ae9n7dHVueyslrb6weq7dTkYDi3iOYRW8HRkIQh06wEdbxt0shTzAJvvCQfrB
jg/3747WSsf/zBTcHihTRBdAv6OmdhV4/dD5YBfLAkLrd+mX7iE=
-----END RSA PRIVATE KEY-----`

func buildAndDecodeRequestTokenPayload(r *IDRequest) (*jot.StandardClaims, error) {
	requestJWT, err := r.requestToken()
	if err != nil {
		return &jot.StandardClaims{}, err
	}
	claims := &jot.StandardClaims{}
	_, err = jot.ParseWithClaims(requestJWT, claims,
		func(t *jot.Token) (interface{}, error) {
			return jwt.ToRsaPubKey(pubKey)
		})
	if err != nil {
		return &jot.StandardClaims{}, err
	}
	return claims, nil
}

func TestCanBuildValidRequestToken(t *testing.T) {
	r := &IDRequest{
		SecondsBeforeExpiry: 10,
		PrivateKey:          privateKey,
	}
	requestJWT, err := r.requestToken()
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	if _, err := jwt.Validate(pubKey, requestJWT); err != nil {
		t.Errorf("should've built a valid JWT: %v", err)
	}
}

func TestCantBuildRequestTokenWithoutPvtKey(t *testing.T) {
	r := &IDRequest{
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

var requestTokenExpClaim = []IDRequest{
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
	r := &IDRequest{PrivateKey: privateKey}
	claims, err := buildAndDecodeRequestTokenPayload(r)
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	now := time.Now().Unix()
	assertClose(t, now, claims.IssuedAt)
}

func TestRequestTokenNbfClaim(t *testing.T) {
	r := &IDRequest{PrivateKey: privateKey}
	claims, err := buildAndDecodeRequestTokenPayload(r)
	if err != nil {
		t.Errorf("should've built a JWT: %v", err)
	}
	now := time.Now().Unix()
	assertClose(t, now, claims.NotBefore)
}

var requestTokenIssAndSubClaims = []IDRequest{
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

var requestTokenAudClaim = []IDRequest{
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
	r := &IDRequest{}
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
