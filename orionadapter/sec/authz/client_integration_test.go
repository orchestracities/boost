package authz

import (
	"testing"
)

// TODO: Get rid of dependency on Gernot's cluster in the future!!
const serverURL = "http://authzforceingress.appstorecontainerns.46.17.108.63.xip.io/authzforce-ce/domains/CYYY_V2IEeqMJKbegCuurA/pdp"

var client = NewClient(serverURL)

func TestPermit(t *testing.T) {
	req := &Request{
		Roles:         []string{"role0", "role1", "role2", "role3"},
		ResourceID:    "b3a4a7d2-ce61-471f-b05d-fb82452ae686",
		ResourcePath:  "/v2",
		FiwareService: "service",
		Action:        "GET",
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
	req := &Request{
		Roles:         []string{},
		ResourceID:    "b3a4a7d2-ce61-471f-b05d-fb82452ae686",
		ResourcePath:  "/v2",
		FiwareService: "service",
		Action:        "GET",
	}
	okay, err := client.Authorize(req)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if okay {
		t.Errorf("unexpected permission granted")
	}
}
