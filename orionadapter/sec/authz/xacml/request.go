package xacml

import (
	"strings"
)

// Request holds the data to do an access control check with the AuthZ server.
type Request struct {
	KeyRockRoles  []string
	IdsScopes     []string
	ConnectorID   string
	DapsIssuer    string
	ResourceID    string
	RequestPath   string
	RequestVerb   string
	FiwareService string
}

// ToXML serializes the request to XACML format.
func (r *Request) ToXML() string {
	ast := r.buildRequestAst()
	return ast.writeDoc()
}

func (r *Request) buildRequestAst() xNode {
	ast := request().children(
		accessSubject().children(
			maybeA(fwKeyRockRole, r.KeyRockRoles),
			idsSecurityProfileAuditLogging(),
			idsSecurityProfilePseudoSecondElement(),
			idsMembership(),
			maybeA(idsScopes, r.IdsScopes),
			maybe(idsDapsIss, r.DapsIssuer),
			maybe(idsDapsSubConnectorCN, r.ConnectorID),
		),
		resource().children(
			maybe(resourceID, r.ResourceID),
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

func maybeA(f func([]string) xNode, vs []string) xNode {
	xs := filterContent(vs)
	if len(xs) > 0 {
		return f(xs)
	}
	return emptyNode()
}
