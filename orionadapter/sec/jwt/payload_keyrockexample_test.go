package jwt

import (
	"reflect"
	"testing"
)

// real-world KeyRock token.
/* Payload:
{
  "organizations": [
    {
      "id": "09d4395e-931c-475c-a723-af362b563b11",
      "name": "Org2",
      "description": "Org2",
      "website": null,
      "roles": [
        {
          "id": "a85e87f8-b7e8-457e-aab1-fbc3a4e88185",
          "name": "Role3"
        }
      ]
    },
    {
      "id": "3ae6a3a3-51f5-4025-98c3-9ceaeb53c0b2",
      "name": "Org1",
      "description": "Org1",
      "website": null,
      "roles": [
        {
          "id": "92c2d37f-580b-47ba-9178-d9ca8ec12378",
          "name": "Role2"
        },
        {
          "id": "a85e87f8-b7e8-457e-aab1-fbc3a4e88185",
          "name": "Role3"
        }
      ]
    }
  ],
  "displayName": "",
  "roles": [
    {
      "id": "93c9e503-05e6-4faf-9e54-4531c0948c62",
      "name": "Role1"
    },
    {
      "id": "92c2d37f-580b-47ba-9178-d9ca8ec12378",
      "name": "Role2"
    }
  ],
  "app_id": "30ccf981-663c-453d-87ee-c1ce36b497aa",
  "trusted_apps": [],
  "isGravatarEnabled": false,
  "image": "",
  "email": "alice-the-admin@test.com",
  "id": "admin",
  "authorization_decision": "",
  "app_azf_domain": "nrFWMH8EEeqBNVYbm7c9rA",
  "eidas_profile": {},
  "attributes": {},
  "username": "alice",
  "type": "user",
  "iat": 1587206636,
  "exp": 1587210236
}
*/
var keyRockJWTExample = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJvcmdhbml6YXRpb25zIjpbeyJpZCI6IjA5ZDQzOTVlLTkzMWMtNDc1Yy1hNzIzLWFmMzYyYjU2M2IxMSIsIm5hbWUiOiJPcmcyIiwiZGVzY3JpcHRpb24iOiJPcmcyIiwid2Vic2l0ZSI6bnVsbCwicm9sZXMiOlt7ImlkIjoiYTg1ZTg3ZjgtYjdlOC00NTdlLWFhYjEtZmJjM2E0ZTg4MTg1IiwibmFtZSI6IlJvbGUzIn1dfSx7ImlkIjoiM2FlNmEzYTMtNTFmNS00MDI1LTk4YzMtOWNlYWViNTNjMGIyIiwibmFtZSI6Ik9yZzEiLCJkZXNjcmlwdGlvbiI6Ik9yZzEiLCJ3ZWJzaXRlIjpudWxsLCJyb2xlcyI6W3siaWQiOiI5MmMyZDM3Zi01ODBiLTQ3YmEtOTE3OC1kOWNhOGVjMTIzNzgiLCJuYW1lIjoiUm9sZTIifSx7ImlkIjoiYTg1ZTg3ZjgtYjdlOC00NTdlLWFhYjEtZmJjM2E0ZTg4MTg1IiwibmFtZSI6IlJvbGUzIn1dfV0sImRpc3BsYXlOYW1lIjoiIiwicm9sZXMiOlt7ImlkIjoiOTNjOWU1MDMtMDVlNi00ZmFmLTllNTQtNDUzMWMwOTQ4YzYyIiwibmFtZSI6IlJvbGUxIn0seyJpZCI6IjkyYzJkMzdmLTU4MGItNDdiYS05MTc4LWQ5Y2E4ZWMxMjM3OCIsIm5hbWUiOiJSb2xlMiJ9XSwiYXBwX2lkIjoiMzBjY2Y5ODEtNjYzYy00NTNkLTg3ZWUtYzFjZTM2YjQ5N2FhIiwidHJ1c3RlZF9hcHBzIjpbXSwiaXNHcmF2YXRhckVuYWJsZWQiOmZhbHNlLCJpbWFnZSI6IiIsImVtYWlsIjoiYWxpY2UtdGhlLWFkbWluQHRlc3QuY29tIiwiaWQiOiJhZG1pbiIsImF1dGhvcml6YXRpb25fZGVjaXNpb24iOiIiLCJhcHBfYXpmX2RvbWFpbiI6Im5yRldNSDhFRWVxQk5WWWJtN2M5ckEiLCJlaWRhc19wcm9maWxlIjp7fSwiYXR0cmlidXRlcyI6e30sInVzZXJuYW1lIjoiYWxpY2UiLCJ0eXBlIjoidXNlciIsImlhdCI6MTU4NzIwNjYzNiwiZXhwIjoxNTg3MjEwMjM2fQ.7ChWs2mSYpQFd7wZti9QCBVYf-_dOXIBj6_cRjZ7tkA"

func TestKeyRockExample(t *testing.T) {
	payload := FromRaw(keyRockJWTExample)

	got := payload.AppID()
	want := "30ccf981-663c-453d-87ee-c1ce36b497aa"
	if got != want {
		t.Errorf("AppID. want: %v; got: %v", want, got)
	}

	got = payload.AppAzfDomain()
	want = "nrFWMH8EEeqBNVYbm7c9rA"
	if got != want {
		t.Errorf("AppAzfDomain. want: %v; got: %v", want, got)
	}

	gotRoles := payload.Roles()
	wantRoles := []string{"Role1", "Role2", "Role3"}
	if !reflect.DeepEqual(wantRoles, gotRoles) {
		t.Errorf("Roles. want: %v; got: %v", wantRoles, gotRoles)
	}

}
