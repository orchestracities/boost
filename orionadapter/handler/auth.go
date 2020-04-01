package handler

import (
	"fmt"

	iad "istio.io/api/mixer/adapter/model/v1beta1"
	"istio.io/istio/mixer/pkg/status"

	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
)

// Authorize tells the Mixer if it should reject the incoming request.
// We go ahead with the request only if it contains a valid IDS-DTH
// token and, if enabled, AuthZ authorizes access to the target resource.
func Authorize(r *od.HandleOrionadapterRequest) (*od.HandleOrionadapterResponse, error) {
	params, err := extractConfig(r)
	if err != nil {
		return err, nil
	}

	claims, err := validateConsumer(r, params)
	if err != nil {
		return err, nil
	}

	providerHeader, err := GenerateProviderHeader(params)
	if err != nil {
		return err, nil
	}

	err = authorizeWithAuthZ(r, params, claims)
	if err != nil {
		return err, nil
	}

	return success(providerHeader), nil
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
