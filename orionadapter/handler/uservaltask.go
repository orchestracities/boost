package handler

import (
	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
	"github.com/orchestracities/boost/orionadapter/sec/jwt"
)

func validateUser(r *od.HandleOrionadapterRequest, params *config.Params) (
	claims jwt.Payload, err *od.HandleOrionadapterResponse) {
	sharedSecret, cErr := getAuthZHs256SharedSecret(params, nil)
	if cErr != nil {
		ilog.Infof("can't read configured AuthZ HS256 shared secret: %v", cErr)
		return nil, configError()
	}

	claims, vErr := jwt.ValidateHMAC(sharedSecret, r.Instance.IdsAuthzToken)
	if vErr != nil {
		ilog.Infof("AuthZ JWT validation failed: %v", vErr)
		return nil, invalidAuthzJWTError()
	}

	return claims, nil
}

func invalidAuthzJWTError() *od.HandleOrionadapterResponse {
	return permissionDeniedResponse("invalid AuthZ JWT data")
}
