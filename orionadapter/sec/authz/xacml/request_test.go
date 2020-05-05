package xacml

import (
	"testing"
)

func TestFullFledgedRequest(t *testing.T) {
	req := &Request{
		KeyRock: KeyRock{
			AppID: "7bc2b735-7fdd-40ea-8cb2-acae9b241ca7",
			Roles: []string{"role1", "role3", "role4"},
		},
		Daps: Daps{
			ConnectorID:            "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
			Issuer:                 "https://daps.aisec.fraunhofer.de",
			Membership:             "true",
			Scopes:                 []string{"ids_connector", "ids_scope_pseudo_second_element"},
			SecProfileAuditLogging: "2",
		},
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
		KeyRock: KeyRock{
			Roles: []string{"", " "},
		},
		Daps: Daps{
			Scopes: []string{""},
		},
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
		KeyRock: KeyRock{
			AppID: "7bc2b735-7fdd-40ea-8cb2-acae9b241ca7",
			Roles: []string{"role1", "role3", "role4"},
		},
		Daps: Daps{
			ConnectorID:            "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
			Issuer:                 "",
			Membership:             "false",
			Scopes:                 []string{"ids_connector", "ids_scope_pseudo_second_element"},
			SecProfileAuditLogging: "3",
		},
		RequestPath:   "/v2/",
		RequestVerb:   "GET",
		FiwareService: "",
	},
	{
		KeyRock: KeyRock{
			AppID: "7bc2b735-7fdd-40ea-8cb2-acae9b241ca7",
			Roles: []string{" ", "role1", "role3", "role4"},
		},
		Daps: Daps{
			ConnectorID:            "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
			Issuer:                 "",
			Membership:             "false",
			Scopes:                 []string{" ", "ids_connector", "ids_scope_pseudo_second_element"},
			SecProfileAuditLogging: "3",
		},
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
