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
	if !validateToken(headerValue) {
		return &iad.CheckResult{
			Status: status.WithPermissionDenied(
				"Unauthorized: invalid JWT token"),
		}, nil
	}
	return &iad.CheckResult{
		Status: status.OK,
	}, nil
}

// TODO: port ballerina code
func validateToken(jwt string) bool {
	if len(jwt) == 0 {
		return false
	}
	return true
}
