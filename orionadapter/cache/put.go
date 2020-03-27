package cache

import (
	"github.com/orchestracities/boost/orionadapter/codegen/config"
	token "github.com/orchestracities/boost/orionadapter/sec"
	"github.com/orchestracities/boost/orionadapter/sec/authz"
)

// PutAdapterConfig caches adapter config. Use the ok flag to tell if the
// operation was successful.
func PutAdapterConfig(params *config.Params) (ok bool) {
	if params == nil {
		return false
	}
	return cached.Config().keep(adapterConfigKey, params)
}

// PutDapsIDToken caches a DAPS ID token. Use the ok flag to tell if the
// operation was successful.
func PutDapsIDToken(jwt string) (ok bool) {
	if len(jwt) == 0 {
		return false
	}
	ttl := token.FromRaw(jwt).ExpiresIn()
	return cached.Daps().put(dapsIDTokenKey, jwt, 1, ttl)
}

// PutAuthZDecision caches an AuthZ decision. Use the ok flag to tell if the
// operation was successful.
func PutAuthZDecision(idsClientHeader string, callParams *authz.Request,
	authorized bool) (ok bool) {
	key, jwt, err := authZCallKey(idsClientHeader, callParams)
	if err != nil {
		return false
	}
	ttl := token.FromRaw(jwt).ExpiresIn()
	return cached.AuthZ().put(key, authorized, 1, ttl)
}
