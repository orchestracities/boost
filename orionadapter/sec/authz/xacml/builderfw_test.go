package xacml

import (
	"testing"
)

func TestFullyFledgedRequest(t *testing.T) {
	req := request().children(
		accessSubject().children(
			fwKeyRockRole([]string{"role1", "role3", "role4"}),
			idsSecurityProfileAuditLogging("2"),
			idsSecurityProfilePseudoSecondElement(),
			idsMembership(true),
			idsScopes([]string{"ids_connector", "ids_scope_pseudo_second_element"}),
			idsDapsIss("https://daps.aisec.fraunhofer.de"),
			idsDapsSubConnectorCN("4e16f007-d959-4eb2-b47d-78dd0c4eab0e"),
		),
		resource().children(
			resourceID("7bc2b735-7fdd-40ea-8cb2-acae9b241ca7"),
			requestPath("/v2/"),
			requestFiwareService("service"),
		),
		action().children(
			requestVerb("GET"),
		),
		environment(),
	)
	got := req.writeDoc()
	if got != fullyFledgedRequest {
		t.Errorf("want: \n%s\ngot: \n%s\n", fullyFledgedRequest, got)
	}
}
