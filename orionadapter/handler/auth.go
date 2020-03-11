package handler

import (
	"fmt"

	iad "istio.io/api/mixer/adapter/model/v1beta1"
	"istio.io/istio/mixer/pkg/status"
	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
	token "github.com/orchestracities/boost/orionadapter/sec"
)

// Authorize tells the Mixer if it should reject the incoming request.
// We go ahead with the request only if it contains a valid IDS-DTH
// token.
func Authorize(r *od.HandleOrionadapterRequest) (*od.HandleOrionadapterResponse, error) {
	ilog.Infof("auth request: %v\n", *r)

	params, err := GetConfig(r)
	pubKeyPemRep, err := getIdsaPublicKey(params, err)
	if err != nil {
		ilog.Errorf("%v", err)
		return configError(), nil
	}

	// props := toMap(r.Instance.Subject.Properties)
	// ilog.Infof("subject props: %v", props)

	// headerValue := getStringValue(props, headerName)
	claims, err := validateToken(pubKeyPemRep, r.Instance.ClientToken)
	if err != nil {
		ilog.Infof("token validation failed: %v", err)
		return invalidJWTError(), nil
	}

	serverToken, err := GenerateToken(params)
	if err != nil {
		ilog.Errorf("error generating server token: %v\n", err)
		return tokenGenError(), nil
	}

	if isAuthZEnabled(params) {

	}

	return success(serverToken), nil
}

func validateToken(pubKey string, headerValue string) (token.JwtPayload, error) {
	jwt, err := token.ReadClientToken(headerValue)
	if err != nil {
		return nil, err
	}
	return token.Validate(pubKey, jwt)
}

// GenerateToken gets a new ID token from DAPS, puts it into the configured
// server header JSON object and then Base64 encodes the JSON object.
func GenerateToken(p *config.Params) (string, error) {
	daps, err := buildDapsIDRequest(p)
	idTokenTemplate, err := getIDTokenJSONTemplate(p, err)
	if err != nil {
		return "", err
	}

	idToken, err := daps.IdentityToken()
	if err != nil {
		return "", err
	}

	return token.BuildServerHeader(idTokenTemplate, idToken)
}

func buildDapsIDRequest(p *config.Params) (*token.DapsIDRequest, error) {
	connectorID, err := getDapsConnectorID(p, nil)
	connectorAudience, err := getDapsConnectorAudience(p, err)
	secondsBeforeExpiry, err := getDapsSecondsBeforeExpiry(p, err)
	privateKey, err := getDapsPrivateKey(p, err)
	connectorCertificate, err := getDapsConnectorCertificate(p, err)
	serverCertificate, err := getDapsServerCertificate(p, err)
	serverHost, err := getDapsServerHost(p, err)

	if err != nil {
		return nil, err
	}

	r := &token.DapsIDRequest{
		ConnectorID:          connectorID,
		ConnectorAudience:    connectorAudience,
		SecondsBeforeExpiry:  secondsBeforeExpiry,
		PrivateKey:           privateKey,
		ConnectorCertificate: connectorCertificate,
		ServerCertificate:    serverCertificate,
		ServerHost:           serverHost,
	}
	return r, nil
}

// response generation boilerplate

func success(serverToken string) *od.HandleOrionadapterResponse {
	return &od.HandleOrionadapterResponse{
		Result: &iad.CheckResult{
			Status: status.OK,
			// ValidDuration: 5 * time.Second
			// i.e. caching? see Keyval
		},
		Output: &od.OutputMsg{ContextBrokerToken: serverToken},
	}
}

func permissionDeniedResponse(reason string) *od.HandleOrionadapterResponse {
	msg := fmt.Sprintf("unauthorized: %s", reason)
	return &od.HandleOrionadapterResponse{
		Result: &iad.CheckResult{
			Status: status.WithPermissionDenied(msg),
		},
	}
}

func adapterErrorResponse(reason string) *od.HandleOrionadapterResponse {
	msg := fmt.Sprintf("adapter error: %s", reason)
	return &od.HandleOrionadapterResponse{
		Result: &iad.CheckResult{
			Status: status.WithUnknown(msg),
		},
	}
}

func invalidJWTError() *od.HandleOrionadapterResponse {
	return permissionDeniedResponse("invalid JWT data")
}

func configError() *od.HandleOrionadapterResponse {
	return adapterErrorResponse("invalid configuration")
}

func tokenGenError() *od.HandleOrionadapterResponse {
	return adapterErrorResponse("context broker token could not be generated")
}
