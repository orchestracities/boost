package handler

import (
	"fmt"

	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/cache"
	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
	"github.com/orchestracities/boost/orionadapter/sec/authz"
	"github.com/orchestracities/boost/orionadapter/sec/authz/xacml"
	"github.com/orchestracities/boost/orionadapter/sec/jwt"
)

type authZCallData struct {
	consumerHeader string
	authzToken     string
	pdpBaseURL     string
	request        *xacml.Request
	cacheMaxSecs   uint64
}

func newAuthZCall(params *config.Params, instance *od.InstanceMsg,
	consumerClaims, userClaims jwt.Payload) (*authZCallData, error) {
	pdpBaseURL, request, err :=
		buildAuthZRequest(params, instance, consumerClaims, userClaims)
	cacheMaxSecs, err := getAuthZCacheDecisionMaxSeconds(params, err)
	if err != nil {
		return nil, err
	}

	return &authZCallData{
		consumerHeader: instance.IdsConsumerHeader,
		authzToken:     instance.IdsAuthzToken,
		pdpBaseURL:     pdpBaseURL,
		request:        request,
		cacheMaxSecs:   cacheMaxSecs,
	}, nil
}

func (d *authZCallData) authZize() (authorized bool, e error) {
	client, err := authz.NewClient(d.pdpBaseURL)
	if err != nil {
		return false, err
	}
	return client.Authorize(d.request)
}

func (d *authZCallData) cachedAuthZDecision() (authorized bool, found bool) {
	return cache.LookupAuthZDecision(d.consumerHeader, d.authzToken, d.request)
}

func (d *authZCallData) cacheAuthZDecision(authorized bool) (ok bool) {
	return cache.PutAuthZDecision(d.consumerHeader, d.authzToken,
		d.request, authorized, d.cacheMaxSecs)
}

func authorizeWithAuthZ(r *od.HandleOrionadapterRequest, params *config.Params,
	consumerClaims jwt.Payload) (err *od.HandleOrionadapterResponse) {
	if isAuthZEnabled(params) {
		return doAuthorizeWithAuthZ(r, params, consumerClaims)
	}
	return nil
}

func doAuthorizeWithAuthZ(r *od.HandleOrionadapterRequest, params *config.Params,
	consumerClaims jwt.Payload) (err *od.HandleOrionadapterResponse) {
	userClaims, err := validateUser(r, params)
	if err != nil {
		return err
	}

	z, zErr := newAuthZCall(params, r.Instance, consumerClaims, userClaims)
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
	consumerClaims, userClaims jwt.Payload) (string, *xacml.Request, error) {
	url, err := getAuthZPdpBaseURL(p, nil)

	return url, &xacml.Request{
		Daps: xacml.Daps{
			ConnectorID: consumerClaims.SubjectCommonName(),
			Issuer:      consumerClaims.Issuer(),
			Membership:  consumerClaims.Membership(),
			Scopes:      consumerClaims.Scopes(),
			SecProfile:  consumerClaims.SecProfile(),
		},
		KeyRock: xacml.KeyRock{
			AppID:        userClaims.AppID(),
			AppAzfDomain: userClaims.AppAzfDomain(),
			Roles:        userClaims.Roles(),
		},
		FiwareService: instance.FiwareService,
		RequestPath:   instance.RequestPath,
		RequestVerb:   instance.RequestMethod,
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
