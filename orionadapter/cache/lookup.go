package cache

import (
	"github.com/orchestracities/boost/orionadapter/codegen/config"
	"github.com/orchestracities/boost/orionadapter/sec/authz"
)

func asBool(value interface{}, found bool) (bool, bool) {
	if found {
		if converted, ok := value.(bool); ok {
			return converted, true
		}
	}
	return false, false
}

func asString(value interface{}, found bool) (string, bool) {
	if found {
		if converted, ok := value.(string); ok {
			return converted, true
		}
	}
	return "", false
}

func asParams(value interface{}, found bool) (*config.Params, bool) {
	if found {
		if converted, ok := value.(*config.Params); ok {
			return converted, true
		}
	}
	return nil, false
}

// LookupAdapterConfig gets any cached adapter config. Use the found flag
// to tell if the lookup was successful.
func LookupAdapterConfig() (params *config.Params, found bool) {
	return asParams(cached.Config().lookup(adapterConfigKey))
}

// LookupDapsIDToken gets any cached DAPS ID token. Use the found flag
// to tell if the lookup was successful.
func LookupDapsIDToken() (token string, found bool) {
	return asString(cached.Daps().lookup(dapsIDTokenKey))
}

// LookupAuthZDecision gets any cached AuthZ decision for the specified
// call parameters. Use the found flag to tell if the lookup was successful.
func LookupAuthZDecision(idsConsumerHeader string, callParams *authz.Request) (
	authorized bool, found bool) {
	key, _, err := authZCallKey(idsConsumerHeader, callParams)
	if err != nil {
		return false, false
	}
	return asBool(cached.AuthZ().lookup(key))
}
