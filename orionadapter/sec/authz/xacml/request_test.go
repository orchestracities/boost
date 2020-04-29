package xacml

import (
	"testing"
)

func TestFullFledgedRequest(t *testing.T) {
	req := &Request{
		KeyRockRoles:  []string{"role1", "role3", "role4"},
		IdsScopes:     []string{"ids_connector", "ids_scope_pseudo_second_element"},
		ConnectorID:   "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
		DapsIssuer:    "https://daps.aisec.fraunhofer.de",
		ResourceID:    "7bc2b735-7fdd-40ea-8cb2-acae9b241ca7",
		RequestPath:   "/v2/",
		RequestVerb:   "GET",
		FiwareService: "service",
	}
	got := req.ToXML()
	if got != fullyFledgedRequest {
		t.Errorf("want: \n%s\ngot: \n%s\n", fullyFledgedRequest, got)
	}
}

var minimalRequestFixtures = []Request{
	{},
	{
		KeyRockRoles:  []string{"", " "},
		IdsScopes:     []string{""},
		ConnectorID:   "",
		DapsIssuer:    "",
		ResourceID:    "",
		RequestPath:   "",
		RequestVerb:   "",
		FiwareService: "",
	},
}

func TestMinimalRequest(t *testing.T) {
	for k, req := range minimalRequestFixtures {
		got := req.ToXML()
		if got != minimalRequest {
			t.Errorf("[%v] want: \n%s\ngot: \n%s\n", k, minimalRequest, got)
		}
	}
}

var requestWithMissingIssuerAndServiceFixtures = []Request{
	{
		KeyRockRoles:  []string{"role1", "role3", "role4"},
		IdsScopes:     []string{"ids_connector", "ids_scope_pseudo_second_element"},
		ConnectorID:   "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
		DapsIssuer:    "",
		ResourceID:    "7bc2b735-7fdd-40ea-8cb2-acae9b241ca7",
		RequestPath:   "/v2/",
		RequestVerb:   "GET",
		FiwareService: "",
	},
	{
		KeyRockRoles:  []string{" ", "role1", "role3", "role4"},
		IdsScopes:     []string{" ", "ids_connector", "ids_scope_pseudo_second_element"},
		ConnectorID:   "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
		DapsIssuer:    "",
		ResourceID:    "7bc2b735-7fdd-40ea-8cb2-acae9b241ca7",
		RequestPath:   "/v2/",
		RequestVerb:   "GET",
		FiwareService: "",
	},
}

func TestRequestWithMissingIssuerAndService(t *testing.T) {
	for k, req := range requestWithMissingIssuerAndServiceFixtures {
		got := req.ToXML()
		if got != requestWithMissingIssuerAndService {
			t.Errorf("[%v] want: \n%s\ngot: \n%s\n",
				k, requestWithMissingIssuerAndService, got)
		}
	}
}
