package handler

import (
	"fmt"
	"time"

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
			Status:        status.OK,
			ValidDuration: 0 * time.Second, // (*)
			ValidUseCount: 1,               // (*)
		},
		Output: &od.OutputMsg{ContextBrokerToken: serverToken},
	}
	// (*) Mixer caching. With those settings we're telling Mixer not to
	// cache our responses. We do this since we've rolled out our own
	// caching. Downside: Mixer will hit us every time an HTTP request
	// comes in. Actually for some obscure reason we get hit twice for
	// each request, but thanks to caching the second call takes about
	// 0.2ms on average. Peanuts, but annoying.
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
