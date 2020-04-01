package handler

import (
	"fmt"

	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/cache"
	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
	"github.com/orchestracities/boost/orionadapter/sec/authz"
	"github.com/orchestracities/boost/orionadapter/sec/jwt"
)

type authZCallData struct {
	consumerHeader string
	serverURL      string
	request        *authz.Request
}

func newAuthZCall(params *config.Params, instance *od.InstanceMsg,
	claims jwt.Payload) (*authZCallData, error) {
	serverURL, request, err := buildAuthZRequest(params, instance, claims)
	if err != nil {
		return nil, err
	}

	return &authZCallData{
		consumerHeader: instance.ClientToken,
		serverURL:      serverURL,
		request:        request,
	}, nil
}

func (d *authZCallData) authZize() (authorized bool, e error) {
	client := authz.NewClient(d.serverURL)
	return client.Authorize(d.request)
}

func (d *authZCallData) cachedAuthZDecision() (authorized bool, found bool) {
	return cache.LookupAuthZDecision(d.consumerHeader, d.request)
}

func (d *authZCallData) cacheAuthZDecision(authorized bool) (ok bool) {
	return cache.PutAuthZDecision(d.consumerHeader, d.request, authorized)
}

func authorizeWithAuthZ(r *od.HandleOrionadapterRequest, params *config.Params,
	claims jwt.Payload) (err *od.HandleOrionadapterResponse) {
	if isAuthZEnabled(params) {
		return doAuthorizeWithAuthZ(r, params, claims)
	}
	return nil
}

func doAuthorizeWithAuthZ(r *od.HandleOrionadapterRequest, params *config.Params,
	claims jwt.Payload) (err *od.HandleOrionadapterResponse) {
	z, zErr := newAuthZCall(params, r.Instance, claims)
	if zErr != nil {
		z.logConfigError(zErr)
		return authzError()
	}

	if authorized, found := z.cachedAuthZDecision(); found {
		if authorized {
			z.logPermitFromCache()
			return nil
		}
		z.logDenyFromCache()
		return authzDeny()
	}

	authorized, zErr := z.authZize()
	if zErr != nil {
		z.logCallError(zErr)
		return authzError()
	}

	decisionCached := z.cacheAuthZDecision(authorized)

	if !authorized {
		z.logDeny(decisionCached)
		return authzDeny()
	}

	z.logPermit(decisionCached)
	return nil
}

func buildAuthZRequest(p *config.Params, instance *od.InstanceMsg,
	claims jwt.Payload) (string, *authz.Request, error) {
	url, err := getAuthZServerURL(p, nil)
	rid, err := getAuthZResourceID(p, err)

	return url, &authz.Request{
		Roles:         claims.Scopes(),
		ResourceID:    rid,
		ResourcePath:  instance.RequestPath,
		FiwareService: instance.FiwareService,
		Action:        instance.RequestMethod,
	}, err
}

// error response generation

func authzError() *od.HandleOrionadapterResponse {
	return adapterErrorResponse("failed to perform AuthZ check")
}

func authzDeny() *od.HandleOrionadapterResponse {
	return permissionDeniedResponse("AuthZ denied authorization")
}

// logging

// message template (no errors)
//
// AuthZ
//   Request: { ResourceID: ..., ..., Action: ... }
//   Decision: Permit | Deny | Permit (cached) | Deny (cached)
//   Caching: decision saved to cache | decision not saved to cache
//
// error message template
//
// AuthZ
//   Request: { ResourceID: ..., ..., Action: ... }
//   Error: ...
//

func (d *authZCallData) logMsgHeader() string {
	h := "AuthZ\n  Request: %+v\n"
	return fmt.Sprintf(h, d.request)
}

func (d *authZCallData) logConfigError(err error) {
	template := "  Error: can't configure call: %v\n"
	errorLine := fmt.Sprintf(template, err)
	msg := d.logMsgHeader() + errorLine
	ilog.Error(msg)
}

func (d *authZCallData) logCallError(err error) {
	template := "  Error: AuthZ call failed: %v\n"
	errorLine := fmt.Sprintf(template, err)
	msg := d.logMsgHeader() + errorLine
	ilog.Error(msg)
}

func (d *authZCallData) logPermitFromCache() {
	decisionLine := "  Decision: Permit (cached)\n"
	msg := d.logMsgHeader() + decisionLine
	ilog.Info(msg)
}

func (d *authZCallData) logDenyFromCache() {
	decisionLine := "  Decision: Deny (cached)\n"
	msg := d.logMsgHeader() + decisionLine
	ilog.Info(msg)
}

func cachingLine(decisionCached bool) string {
	if decisionCached {
		return "  Caching: decision saved to cache\n"
	}
	return "  Caching: decision not saved to cache\n"
}

func (d *authZCallData) logDeny(decisionCached bool) {
	decisionLine := "  Decision: Deny\n"
	msg := d.logMsgHeader() + decisionLine + cachingLine(decisionCached)
	ilog.Info(msg)
}

func (d *authZCallData) logPermit(decisionCached bool) {
	decisionLine := "  Decision: Permit\n"
	msg := d.logMsgHeader() + decisionLine + cachingLine(decisionCached)
	ilog.Info(msg)
}
