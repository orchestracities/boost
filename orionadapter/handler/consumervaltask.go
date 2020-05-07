package handler

import (
	"errors"
	"fmt"

	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
	"github.com/orchestracities/boost/orionadapter/sec/consumer"
	"github.com/orchestracities/boost/orionadapter/sec/jwt"
)

func validateConsumer(r *od.HandleOrionadapterRequest, params *config.Params) (
	claims jwt.Payload, err *od.HandleOrionadapterResponse) {
	pubKeyPemRep, cErr := getIdsaPublicKey(params, nil)
	if cErr != nil {
		ilog.Infof("can't read configured consumer pub key: %v", cErr)
		return nil, configError()
	}

	claims, vErr := validateToken(pubKeyPemRep, r.Instance.IdsConsumerHeader)
	validateIssuer(r.Instance.IdsConsumerHeader, claims, vErr)
	if vErr != nil {
		ilog.Infof("consumer JWT validation failed: %v", vErr)
		return nil, invalidJWTError()
	}

	return claims, nil
}

func validateToken(pubKey string, headerValue string) (jwt.Payload, error) {
	jwtData, err := consumer.ReadToken(headerValue)
	if err != nil {
		return nil, err
	}
	return jwt.Validate(pubKey, jwtData)
}

func validateIssuer(headerValue string, claims jwt.Payload, err error) error {
	if err != nil {
		return err
	}

	headerIssuerID, err := consumer.ReadIssuerConnectorID(headerValue)
	if err != nil {
		return err
	}

	if headerIssuerID != claims.SubjectCommonName() {
		msg := fmt.Sprintf("issuer connector ID (%v) != subject CN (%v)",
			headerIssuerID, claims.SubjectCommonName())
		return errors.New(msg)
	}
	return nil
}

func invalidJWTError() *od.HandleOrionadapterResponse {
	return permissionDeniedResponse("invalid consumer JWT data")
}
