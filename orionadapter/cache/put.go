package cache

import (
	"github.com/orchestracities/boost/orionadapter/codegen/config"
	"github.com/orchestracities/boost/orionadapter/sec/authz/xacml"
	"github.com/orchestracities/boost/orionadapter/sec/jwt"
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
func PutDapsIDToken(jwtData string) (ok bool) {
	if len(jwtData) == 0 {
		return false
	}
	ttl := jwt.FromRaw(jwtData).ExpiresIn()
	return cached.Daps().put(dapsIDTokenKey, jwtData, 1, ttl)
}

// PutAuthZDecision caches an AuthZ decision. Use the ok flag to tell if the
// operation was successful.
func PutAuthZDecision(idsConsumerHeader string, callParams *xacml.Request,
	authorized bool) (ok bool) {
	key, jwtData, err := authZCallKey(idsConsumerHeader, callParams)
	if err != nil {
		return false
	}
	ttl := jwt.FromRaw(jwtData).ExpiresIn()
	return cached.AuthZ().put(key, authorized, 1, ttl)
}
