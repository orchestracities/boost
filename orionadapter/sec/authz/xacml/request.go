package xacml

import (
	"strconv"
	"strings"
)

// Request holds the data to do an access control check with the AuthZ server.
type Request struct {
	Daps          Daps
	KeyRock       KeyRock
	FiwareService string
	RequestPath   string
	RequestVerb   string
}

// Daps holds the DAPS-specific data to do an access control check with the
// AuthZ server.
type Daps struct {
	ConnectorID            string
	Issuer                 string
	Membership             string
	Scopes                 []string
	SecProfileAuditLogging string
}

// KeyRock holds the KeyRock-specific data to do an access control check
// with the AuthZ server.
type KeyRock struct {
	AppID        string
	AppAzfDomain string
	Roles        []string
}

// ToXML serializes the request to XACML format.
func (r *Request) ToXML() string {
	ast := r.buildRequestAst()
	return ast.writeDoc()
}

func (r *Request) buildRequestAst() xNode {
	ast := request().children(
		accessSubject().children(
			maybeA(fwKeyRockRole, r.KeyRock.Roles),
			maybe(idsSecurityProfileAuditLogging, r.Daps.SecProfileAuditLogging),
			idsSecurityProfilePseudoSecondElement(),
			maybeBool(idsMembership, r.Daps.Membership),
			maybeA(idsScopes, r.Daps.Scopes),
			maybe(idsDapsIss, r.Daps.Issuer),
			maybe(idsDapsSubConnectorCN, r.Daps.ConnectorID),
		),
		resource().children(
			maybe(resourceID, r.KeyRock.AppID),
			maybe(requestPath, r.RequestPath),
			maybe(requestFiwareService, r.FiwareService),
		),
		action().children(
			maybe(requestVerb, r.RequestVerb),
		),
		environment(),
	)
	return ast
}

func hasContent(x string) bool {
	trimmed := strings.TrimSpace(x)
	return trimmed != ""
}

func filterContent(xs []string) []string {
	ys := make([]string, 0, len(xs))
	for _, x := range xs {
		if hasContent(x) {
			ys = append(ys, x)
		}
	}
	return ys
}

func maybe(f func(string) xNode, v string) xNode {
	if hasContent(v) {
		return f(v)
	}
	return emptyNode()
}

func maybeBool(f func(bool) xNode, v string) xNode {
	if hasContent(v) {
		if b, err := strconv.ParseBool(v); err == nil {
			return f(b)
		}
	}
	return emptyNode()
}

func maybeA(f func([]string) xNode, vs []string) xNode {
	xs := filterContent(vs)
	if len(xs) > 0 {
		return f(xs)
	}
	return emptyNode()
}
