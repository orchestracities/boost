package handler

import (
	iad "istio.io/api/mixer/adapter/model/v1beta1"
	"istio.io/istio/mixer/pkg/status"
	ilog "istio.io/pkg/log"

	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
)

// Authorize tells the Mixer if it should reject the incoming request.
// We go ahead with the request only if it contains a valid IDS-DTH
// token.
func Authorize(r *od.HandleOrionadapterRequest) (*od.HandleOrionadapterResponse, error) {
	ilog.Infof("auth request: %v\n", *r)

	params, err := getConfig(r)
	headerName, err := getIdsDthHeader(params, err)
	if len(headerName) == 0 {
		return &od.HandleOrionadapterResponse{
			Result: &iad.CheckResult{
				Status: status.WithPermissionDenied(
					"Unauthorized: no IDS-DTH header configured"),
			},
		}, nil
	}

	// props := toMap(r.Instance.Subject.Properties)
	// ilog.Infof("subject props: %v", props)

	// headerValue := getStringValue(props, headerName)
	headerValue := r.Instance.ClientToken
	expectedToken, err := getIdsDthExpectedToken(params, nil)
	if !validateToken(headerValue, expectedToken) {
		return &od.HandleOrionadapterResponse{
			Result: &iad.CheckResult{
				Status: status.WithPermissionDenied(
					"Unauthorized: invalid JWT token"),
			},
		}, nil
	}

	serverToken, err := generateToken()
	if err != nil {
		ilog.Errorf("error generating server token: %v\n", err)
		return &od.HandleOrionadapterResponse{
			Result: &iad.CheckResult{
				Status: status.WithUnknown(
					"Context broker token could not be generated"),
			},
		}, nil
	}

	return &od.HandleOrionadapterResponse{
		Result: &iad.CheckResult{
			Status: status.OK,
			// ValidDuration: 5 * time.Second
			// i.e. caching? see Keyval
		},
		Output: &od.OutputMsg{ContextBrokerToken: serverToken},
	}, nil
}

// TODO: port ballerina code and get rid of expected_token param
func validateToken(jwt string, expectedToken string) bool {
	if jwt == expectedToken {
		return true
	}
	return false
}

func generateToken() (string, error) {
	return "generated.server.token", nil
}
