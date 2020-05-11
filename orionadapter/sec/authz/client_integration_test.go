package authz

import (
	"testing"

	"github.com/orchestracities/boost/orionadapter/sec/authz/xacml"
)

// TODO: Get rid of dependency on Gernot's cluster in the future!!
const pdpBaseURL = "http://authzforceingress.appstorecontainerns.46.17.108.63.xip.io/authzforce-ce/domains"

var client, _ = NewClient(pdpBaseURL)

func TestPermit(t *testing.T) {
	req := &xacml.Request{
		KeyRock: xacml.KeyRock{
			AppID:        "7bc2b735-7fdd-40ea-8cb2-acae9b241ca7",
			AppAzfDomain: "wCIwcYFkEeqBNVYbm7c9rA",
			Roles:        []string{"role1", "role3", "role4"},
		},
		Daps: xacml.Daps{
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
	okay, err := client.Authorize(req)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !okay {
		t.Errorf("unexpected permission denied")
	}
}

func TestDeny(t *testing.T) {
	req := &xacml.Request{
		KeyRock: xacml.KeyRock{
			AppID:        "7bc2b735-7fdd-40ea-8cb2-acae9b241ca7",
			AppAzfDomain: "wCIwcYFkEeqBNVYbm7c9rA",
		},
		Daps: xacml.Daps{
			ConnectorID:            "4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
			Issuer:                 "https://daps.aisec.fraunhofer.de",
			Membership:             "true",
			Scopes:                 []string{"ids_connector", "ids_scope_pseudo_second_element"},
			SecProfileAuditLogging: "2",
		},
		FiwareService: "service",
		RequestPath:   "/v2",
		RequestVerb:   "GET",
	}
	okay, err := client.Authorize(req)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if okay {
		t.Errorf("unexpected permission granted")
	}
}
