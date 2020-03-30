package handler

import (
	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
	"github.com/orchestracities/boost/orionadapter/sec/authz"
	"github.com/orchestracities/boost/orionadapter/sec/jwt"
)

func authorizeWithAuthZ(r *od.HandleOrionadapterRequest, params *config.Params,
	claims jwt.Payload) (err *od.HandleOrionadapterResponse) {
	if isAuthZEnabled(params) {
		authorized, zErr := doAuthorizeWithAuthZ(params, r.Instance, claims)
		if zErr != nil {
			ilog.Errorf("error authorizing with AuthZ server: %v\n", zErr)
			return authzError()
		}

		if !authorized {
			ilog.Infof("AuthZ denied access to resource at: %v\n",
				r.Instance.RequestPath)
			return authzDeny()
		}

		ilog.Infof("AuthZ authorized access to resource at: %v\n",
			r.Instance.RequestPath)
	}
	return nil
}

func doAuthorizeWithAuthZ(p *config.Params, instance *od.InstanceMsg,
	claims jwt.Payload) (bool, error) {
	serverURL, request, err := buildAuthZRequest(p, instance, claims)
	if err != nil {
		return false, err
	}

	ilog.Infof("requesting permission from AuthZ: %+v\n", request)

	client := authz.NewClient(serverURL)
	return client.Authorize(request)
}

func buildAuthZRequest(p *config.Params, instance *od.InstanceMsg,
	claims jwt.Payload) (string, *authz.Request, error) {
	url, err := getAuthZServerURL(p, nil)
	rid, err := getAuthZResourceID(p, err)
	if err != nil {
		return "", nil, err
	}

	return url, &authz.Request{
		Roles:         claims.Scopes(),
		ResourceID:    rid,
		ResourcePath:  instance.RequestPath,
		FiwareService: instance.FiwareService,
		Action:        instance.RequestMethod,
	}, nil
}

func authzError() *od.HandleOrionadapterResponse {
	return adapterErrorResponse("failed to perform AuthZ check")
}

func authzDeny() *od.HandleOrionadapterResponse {
	return permissionDeniedResponse("AuthZ denied authorization")
}
