package jwt

import (
	"reflect"
	"testing"
	"time"

	jot "github.com/dgrijalva/jwt-go"
)

func TestFromMapClaimsWithNil(t *testing.T) {
	payload := fromMapClaims(nil)
	if payload == nil {
		t.Errorf("want: empty payload; got: nil")
	}
	if size := len(payload); size != 0 {
		t.Errorf("want: empty payload; got size: %v", size)
	}
}

func TestFromMapClaimsWithZeroValue(t *testing.T) {
	token := &jot.Token{}
	payload := fromMapClaims(token)
	if payload == nil {
		t.Errorf("want: empty payload; got: nil")
	}
	if size := len(payload); size != 0 {
		t.Errorf("want: empty payload; got size: %v", size)
	}
}

func TestFromMapClaimsWithIncompatibleType(t *testing.T) {
	token := &jot.Token{
		Claims: &jot.StandardClaims{
			Subject: "foo",
		},
	}
	payload := fromMapClaims(token)
	if payload == nil {
		t.Errorf("want: empty payload; got: nil")
	}
	if size := len(payload); size != 0 {
		t.Errorf("want: empty payload; got size: %v", size)
	}
}

func TestFromMapClaimsWithEntries(t *testing.T) {
	token := &jot.Token{
		Claims: jot.MapClaims{
			"Subject": "foo",
		},
	}
	payload := fromMapClaims(token)
	if payload == nil {
		t.Errorf("want: one-entry payload; got: nil")
	}
	if size := len(payload); size != 1 {
		t.Errorf("want: one-entry payload; got size: %v", size)
	}
	if value := payload["Subject"]; value != "foo" {
		t.Errorf("want: foo; got: %v", value)
	}
}

var fromRawMalformedFixtures = []string{
	"", ".", "..",
	// derived from { alg: RS256 }.{ }.sig by chopping off part of payload
	`eyJhbGciOiJSUzI1NiJ9.e.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw`,
	// same header & sig as above, but now payload: '{ in: valid! }'
	`eyJhbGciOiJSUzI1NiJ9.eyBpbjogdmFsaWQhIH0.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw`,
}

func TestFromRawMalformed(t *testing.T) {
	for k, d := range fromRawMalformedFixtures {
		got := FromRaw(d)
		if !got.IsEmpty() {
			t.Errorf("[%v] want: empty; got: %v", k, got)
		}
	}
}

var fromRawWithInvalidSignatureFixtures = []string{
	// { alg: HS256 }.{ scopes = [a, b], exp: 123.1 }.sig
	// sig is same as that for { alg: RS256 }.{} so it's not valid
	`eyJhbGciOiJIUzI1NiJ9.eyJzY29wZXMiOlsiYSIsImIiXSwiZXhwIjoxMjMuMX0.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw`,
	// { alg: HS256 }.{ scopes = [a, b], exp: 123.1 }.junk_sig
	`eyJhbGciOiJIUzI1NiJ9.eyJzY29wZXMiOlsiYSIsImIiXSwiZXhwIjoxMjMuMX0.junk_sig`,
	// { alg: HS256 }.{ scopes = [a, b], exp: 123 }.junk_sig
	`eyJhbGciOiJIUzI1NiJ9.eyJzY29wZXMiOlsiYSIsImIiXSwiZXhwIjoxMjN9.junk_sig`,
}

func TestFromRawWithInvalidSignature(t *testing.T) {
	for k, d := range fromRawWithInvalidSignatureFixtures {
		wantScopes := []string{"a", "b"}
		wantExp := uint64(123)
		got := FromRaw(d)
		if !reflect.DeepEqual(wantScopes, got.Scopes()) {
			t.Errorf("[%v] want scopes: %v; got: %v", k, wantScopes, got)
		}
		if wantExp != got.ExpirationTime() {
			t.Errorf("[%v] want exp: %v; got: %v", k, wantExp, got)
		}
	}
}

var expiresInFixtures = []struct {
	exp interface{}
	lo  uint64
	hi  uint64
}{
	{-12.3, 0, 0}, {0, 0, 0}, {time.Now().Unix(), 0, 0},
	{float64(time.Now().Unix()) + 0.1, 0, 0},
	{time.Now().Unix() + 10, 5, 10},
}

func TestExpiresIn(t *testing.T) {
	for k, d := range expiresInFixtures {
		payload := Payload{
			"exp": d.exp,
		}
		got := payload.ExpiresIn()
		if got < d.lo || got > d.hi {
			t.Errorf("[%v] want: ∈ [%v, %v]; got: %v", k, d.lo, d.hi, got)
		}
	}
}

func TestExpiresInWhenMissingExp(t *testing.T) {
	payload := Payload{}
	got := payload.ExpiresIn()
	want := uint64(0)
	if got != want {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

func TestExpirationTimeWhenMissingExp(t *testing.T) {
	payload := Payload{}
	got := payload.ExpirationTime()
	want := uint64(0)
	if got != want {
		t.Errorf("want: %v; got: %v", want, got)
	}
}
