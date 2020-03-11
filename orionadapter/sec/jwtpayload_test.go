package token

import (
	"testing"

	"github.com/dgrijalva/jwt-go"
)

func TestFromMapClaimsWithZeroValue(t *testing.T) {
	token := &jwt.Token{}
	payload := fromMapClaims(token)
	if payload == nil {
		t.Errorf("want: empty payload; got: nil")
	}
	if size := len(payload); size != 0 {
		t.Errorf("want: empty payload; got size: %v", size)
	}
}

func TestFromMapClaimsWithIncompatibleType(t *testing.T) {
	token := &jwt.Token{
		Claims: &jwt.StandardClaims{
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
	token := &jwt.Token{
		Claims: jwt.MapClaims{
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

func TestMissingScopes(t *testing.T) {
	payload := JwtPayload{}
	got := payload.Scopes()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestScopesWithEmptyArray(t *testing.T) {
	payload := JwtPayload{
		"scopes": []string{},
	}
	got := payload.Scopes()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestScopesWithIncompatibleType(t *testing.T) {
	payload := JwtPayload{
		"scopes": []int{},
	}
	got := payload.Scopes()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestScopesWithSomeValues(t *testing.T) {
	payload := JwtPayload{
		"scopes": []string{"a", "b"},
	}
	got := payload.Scopes()
	if got == nil {
		t.Errorf("want: two-entry slice; got: nil")
	}
	if size := len(got); size != 2 {
		t.Errorf("want: two-entry slice; got: %v", got)
	}
	if got[0] != "a" {
		t.Errorf("want: a; got: %v", got[0])
	}
	if got[1] != "b" {
		t.Errorf("want: b; got: %v", got[1])
	}
}
