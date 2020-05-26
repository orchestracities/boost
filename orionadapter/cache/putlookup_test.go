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
	authzToken := expIn2050JWT
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")
	authorized := true
	cacheMax := uint64(3600)

	if ok := PutAuthZDecision(
		header, authzToken, params, authorized, cacheMax); !ok {
		t.Error("failed to put authz decision")
	}

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	got, found := LookupAuthZDecision(header, authzToken, params)
	if !found {
		t.Errorf("authz decision not found, got: %v", got)
	}
	if got != authorized {
		t.Errorf("want: %v; got: %v", authorized, got)
	}
}

func assertDropAuthZDecision(t *testing.T,
	consumerHeader, userJWT string, maxCache uint64, reasonToDrop string) {
	ensureInit(t)

	params := callParams([]string{"r1"}, "/v2", "svc", "GET")
	authorized := true

	if ok := PutAuthZDecision(
		consumerHeader, userJWT, params, authorized, maxCache); ok {
		t.Error(reasonToDrop)
	}

	got, found := LookupAuthZDecision(consumerHeader, userJWT, params)
	if found {
		t.Errorf("authz decision found: %v", got)
	}
}

func TestDropAuthZDecisionWhenConsumerTokenExpired(t *testing.T) {
	consumerHeader := clientJSONPayload(expiredJWT)
	authzToken := expIn2050JWT
	maxCache := uint64(3600)
	reason := "should refuse to put authz decision if consumer JWT has expired"

	assertDropAuthZDecision(t, consumerHeader, authzToken, maxCache, reason)
}

func TestDropAuthZDecisionWhenUserTokenExpired(t *testing.T) {
	consumerHeader := clientJSONPayload(expIn2050JWT)
	authzToken := expiredJWT
	maxCache := uint64(3600)
	reason := "should refuse to put authz decision if user JWT has expired"

	assertDropAuthZDecision(t, consumerHeader, authzToken, maxCache, reason)
}

func TestDropAuthZDecisionWhenConsumerTokenWithNoExp(t *testing.T) {
	consumerHeader := clientJSONPayload(emptyJWT)
	authzToken := expIn2050JWT
	maxCache := uint64(3600)
	reason := "should refuse to put authz decision if consumer JWT has no exp"

	assertDropAuthZDecision(t, consumerHeader, authzToken, maxCache, reason)
}

func TestDropAuthZDecisionWhenUserTokenWithNoExp(t *testing.T) {
	consumerHeader := clientJSONPayload(expIn2050JWT)
	authzToken := emptyJWT
	maxCache := uint64(3600)
	reason := "should refuse to put authz decision if user JWT has no exp"

	assertDropAuthZDecision(t, consumerHeader, authzToken, maxCache, reason)
}

func TestDropAuthZDecisionWhenInvalidConsumerToken(t *testing.T) {
	consumerHeader := "this is so wrong"
	authzToken := expIn2050JWT
	maxCache := uint64(3600)
	reason := "should refuse to put authz decision with invalid consumer JWT"

	assertDropAuthZDecision(t, consumerHeader, authzToken, maxCache, reason)
}

func TestDropAuthZDecisionWhenNoMaxCacheParam(t *testing.T) {
	consumerHeader := "this is so wrong"
	authzToken := expIn2050JWT
	maxCache := uint64(0)
	reason := "should refuse to put authz decision if max cache = 0"

	assertDropAuthZDecision(t, consumerHeader, authzToken, maxCache, reason)
}

func TestNoCachedAuthZDecision(t *testing.T) {
	ensureInit(t)

	header := clientJSONPayload(expIn2050JWT)
	authzToken := expIn2050JWT
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")

	got, found := LookupAuthZDecision(header, authzToken, params)
	if found {
		t.Errorf("authz decision found: %v", got)
	}
}
