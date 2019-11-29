package handler

import (
	iad "istio.io/api/mixer/adapter/model/v1beta1"
	"istio.io/istio/mixer/pkg/status"
	iauth "istio.io/istio/mixer/template/authorization"
	ilog "istio.io/pkg/log"
)

// Authorize tells the Mixer if it should reject the incoming request.
// We go ahead with the request only if it contains a valid IDS-DTH
// token.
func Authorize(r *iauth.HandleAuthorizationRequest) (*iad.CheckResult, error) {
	ilog.Infof("auth request: %v\n", *r)

	params, err := getConfig(r)
	headerName, err := getIdsDthHeader(params, err)
	if len(headerName) == 0 {
		return &iad.CheckResult{
			Status: status.WithPermissionDenied(
				"Unauthorized: no IDS-DTH header configured"),
		}, nil
	}

	props := toMap(r.Instance.Subject.Properties)
	ilog.Infof("subject props: %v", props)

	headerValue := getStringValue(props, headerName)
	expectedToken, err := getIdsDthExpectedToken(params, nil)
	if !validateToken(headerValue, expectedToken) {
		return &iad.CheckResult{
			Status: status.WithPermissionDenied(
				"Unauthorized: invalid JWT token"),
		}, nil
	}
	return &iad.CheckResult{
		Status: status.OK,
	}, nil
}

// TODO: port ballerina code and get rid of expected_token param
func validateToken(jwt string, expectedToken string) bool {
	if jwt == expectedToken {
		return true
	}
	return false
}

// TODO: remove ids-dht header b/f forwarding msg to orion!
