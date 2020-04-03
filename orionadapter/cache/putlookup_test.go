package cache

import (
	"testing"
	"time"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
)

func ensureInit(t *testing.T) {
	if err := Init(); err != nil {
		t.Errorf("failed to initialize cache: %v", err)
	}
}

func TestStoreAndRetrieveAdapterConfig(t *testing.T) {
	ensureInit(t)

	cfg := &config.Params{IdsaPublicKey: "k"}
	if ok := PutAdapterConfig(cfg); !ok {
		t.Error("failed to put config")
	}

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	got, found := LookupAdapterConfig()
	if !found {
		t.Errorf("config not found, got: %v", got)
	}
	if got != cfg { // ptr comp, both must point to same place on heap
		t.Errorf("want: %v; got: %v", cfg, got)
	}
	if got.IdsaPublicKey != cfg.IdsaPublicKey {
		t.Errorf("want: %v; got: %v", cfg.IdsaPublicKey, got.IdsaPublicKey)
	}
}

func TestDropNilAdapterConfig(t *testing.T) {
	ensureInit(t)

	cfg := &config.Params{IdsaPublicKey: "k"}
	if ok := PutAdapterConfig(cfg); !ok {
		t.Error("failed to put config")
	}
	if ok := PutAdapterConfig(nil); ok {
		t.Error("should've refused to put nil config")
	}

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	got, found := LookupAdapterConfig()
	if !found {
		t.Errorf("old config not found, got: %v", got)
	}
	if got != cfg { // ptr comp, both must point to same place on heap
		t.Errorf("want old config: %v; got: %v", cfg, got)
	}
}

func TestNoCachedAdapterConfig(t *testing.T) {
	ensureInit(t)

	got, found := LookupAdapterConfig()
	if found {
		t.Errorf("no config but found: %v", got)
	}
}

// tokens generated on jwt.io
//                     { alg: HS256 }.{}.sig
const emptyJWT = `eyJhbGciOiJIUzI1NiJ9.e30.ZRrHA1JJJW8opsbCGfG_HACGpVUMN_a9IV7pAx_Zmeo`

//                     { alg: HS256 }.{ exp: 10 }.sig
const expiredJWT = `eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjEwfQ.aPE8oUjuqjmcV-jE_Z9tEiaEn-4wpIjjHY7kzxtB85Q`

//                     { alg: HS256 }.{ exp: 2524608000 }.sig
//                                          ^ 01 Jan 2050
const expIn2050JWT = `eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjI1MjQ2MDgwMDB9.e2QtHNDKYdYvbdu95KY6xfcHppvkSOuNBhx2Y-8XYQY`

func TestStoreAndRetrieveDapsIDToken(t *testing.T) {
	ensureInit(t)

	if ok := PutDapsIDToken(expIn2050JWT); !ok {
		t.Error("failed to put daps id token")
	}

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	got, found := LookupDapsIDToken()
	if !found {
		t.Errorf("daps id token not found, got: %v", got)
	}
	if got != expIn2050JWT {
		t.Errorf("want: %v; got: %v", expIn2050JWT, got)
	}
}

func TestDropExpiredDapsIDToken(t *testing.T) {
	ensureInit(t)

	if ok := PutDapsIDToken(expiredJWT); ok {
		t.Error("should've refused to put expired daps id token")
	}

	got, found := LookupDapsIDToken()
	if found {
		t.Errorf("daps id token found: %v", got)
	}
}

func TestDropDapsIDTokenWithNoExp(t *testing.T) {
	ensureInit(t)

	if ok := PutDapsIDToken(emptyJWT); ok {
		t.Error("should've refused to put daps id token with no exp")
	}

	got, found := LookupDapsIDToken()
	if found {
		t.Errorf("daps id token found: %v", got)
	}
}

func TestDropEmptyStringDapsIDToken(t *testing.T) {
	ensureInit(t)

	if ok := PutDapsIDToken(expIn2050JWT); !ok {
		t.Error("failed to put daps id token")
	}
	if ok := PutDapsIDToken(""); ok {
		t.Error("should've refused to put empty string")
	}

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	got, found := LookupDapsIDToken()
	if !found {
		t.Errorf("old daps id token not found, got: %v", got)
	}
	if got != expIn2050JWT {
		t.Errorf("want old daps id token: %v; got: %v", expIn2050JWT, got)
	}
}

func TestNoCachedDapsIDToken(t *testing.T) {
	ensureInit(t)

	got, found := LookupDapsIDToken()
	if found {
		t.Errorf("daps id token found: %v", got)
	}
}

func TestStoreAndRetrieveAuthZDecision(t *testing.T) {
	ensureInit(t)

	header := clientJSONPayload(expIn2050JWT)
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")
	authorized := true

	if ok := PutAuthZDecision(header, params, authorized); !ok {
		t.Error("failed to put authz decision")
	}

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	got, found := LookupAuthZDecision(header, params)
	if !found {
		t.Errorf("authz decision not found, got: %v", got)
	}
	if got != authorized {
		t.Errorf("want: %v; got: %v", authorized, got)
	}
}

func TestDropAuthZDecisionWhenClientTokenExpired(t *testing.T) {
	ensureInit(t)

	header := clientJSONPayload(expiredJWT)
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")
	authorized := true

	if ok := PutAuthZDecision(header, params, authorized); ok {
		t.Error("should refuse to put authz decision if JWT has expired")
	}

	got, found := LookupAuthZDecision(header, params)
	if found {
		t.Errorf("authz decision found: %v", got)
	}
}

func TestDropAuthZDecisionWhenClientTokenWithNoExp(t *testing.T) {
	ensureInit(t)

	header := clientJSONPayload(emptyJWT)
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")
	authorized := true

	if ok := PutAuthZDecision(header, params, authorized); ok {
		t.Error("should refuse to put authz decision if JWT has no exp")
	}

	got, found := LookupAuthZDecision(header, params)
	if found {
		t.Errorf("authz decision found: %v", got)
	}
}

func TestDropAuthZDecisionWhenInvalidClientToken(t *testing.T) {
	ensureInit(t)

	header := "this is so wrong"
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")
	authorized := true

	if ok := PutAuthZDecision(header, params, authorized); ok {
		t.Error("should refuse to put authz decision with invalid JWT")
	}

	got, found := LookupAuthZDecision(header, params)
	if found {
		t.Errorf("authz decision found: %v", got)
	}
}

func TestNoCachedAuthZDecision(t *testing.T) {
	ensureInit(t)

	header := clientJSONPayload(expIn2050JWT)
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")

	got, found := LookupAuthZDecision(header, params)
	if found {
		t.Errorf("authz decision found: %v", got)
	}
}
