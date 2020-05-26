package jwt

import (
	"reflect"
	"testing"
)

func TestMissingRoles(t *testing.T) {
	payload := Payload{}
	got := payload.Roles()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestRolesWithEmptyRolesAndOrgArrays(t *testing.T) {
	payload := Payload{
		"organizations": []interface{}{},
		"roles":         []interface{}{},
	}
	got := payload.Roles()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestRolesWithIncompatibleType(t *testing.T) {
	payload := Payload{
		"organizations": []string{"r0"},
		"roles":         []string{"r1"},
	}
	got := payload.Roles()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestRolesWithSomeNames(t *testing.T) {
	role := []map[string]interface{}{
		{},
		{"id": 1, "name": "r1"},
		{"id": 2, "name": nil},
		{"id": 3},
		{"id": 4, "name": "r4"},
		{"id": 5, "name": ""},
		{"name": "r6"},
		{"id": 7, "name": "r7"},
	}
	org := []map[string]interface{}{
		{},
		{
			"name":  "org1",
			"roles": []interface{}{role[1], role[2], role[3]},
		},
		{
			"name":  "org2",
			"roles": []interface{}{role[4], role[5], role[6]},
		},
	}
	payload := Payload{
		"organizations": []interface{}{org[0], org[1], org[2]},
		"roles":         []interface{}{role[7], role[6], role[0]},
	}
	want := []string{"r1", "r4", "r6", "r7"}
	got := payload.Roles()

	if !reflect.DeepEqual(want, got) {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

// jot is a JWT generated on jwt.io using keys in validation_test.go
func extractRolesFromJwt(t *testing.T, jot string) []string {
	payload, err := Validate(pubKey, jot)
	if err != nil {
		t.Errorf("unexpected JWT validation error: %v", err)
	}
	return payload.Roles()
}

// tokens generated on jwt.io with keys in validation_test.go
var extractRolesFromJwtFixtures = []struct {
	token string
	want  []string
}{
	{
		// { alg: RS256 }.{ }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.e30.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw`,
		want:  []string{},
	},
	{
		// real-world, complex key rock token, decode on jwt.io
		token: `eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJvcmdhbml6YXRpb25zIjpbeyJpZCI6IjA5ZDQzOTVlLTkzMWMtNDc1Yy1hNzIzLWFmMzYyYjU2M2IxMSIsIm5hbWUiOiJPcmcyIiwiZGVzY3JpcHRpb24iOiJPcmcyIiwid2Vic2l0ZSI6bnVsbCwicm9sZXMiOlt7ImlkIjoiYTg1ZTg3ZjgtYjdlOC00NTdlLWFhYjEtZmJjM2E0ZTg4MTg1IiwibmFtZSI6IlJvbGUzIn1dfSx7ImlkIjoiM2FlNmEzYTMtNTFmNS00MDI1LTk4YzMtOWNlYWViNTNjMGIyIiwibmFtZSI6Ik9yZzEiLCJkZXNjcmlwdGlvbiI6Ik9yZzEiLCJ3ZWJzaXRlIjpudWxsLCJyb2xlcyI6W3siaWQiOiI5MmMyZDM3Zi01ODBiLTQ3YmEtOTE3OC1kOWNhOGVjMTIzNzgiLCJuYW1lIjoiUm9sZTIifSx7ImlkIjoiYTg1ZTg3ZjgtYjdlOC00NTdlLWFhYjEtZmJjM2E0ZTg4MTg1IiwibmFtZSI6IlJvbGUzIn1dfV0sImRpc3BsYXlOYW1lIjoiIiwicm9sZXMiOlt7ImlkIjoiOTNjOWU1MDMtMDVlNi00ZmFmLTllNTQtNDUzMWMwOTQ4YzYyIiwibmFtZSI6IlJvbGUxIn0seyJpZCI6IjkyYzJkMzdmLTU4MGItNDdiYS05MTc4LWQ5Y2E4ZWMxMjM3OCIsIm5hbWUiOiJSb2xlMiJ9XSwiYXBwX2lkIjoiMzBjY2Y5ODEtNjYzYy00NTNkLTg3ZWUtYzFjZTM2YjQ5N2FhIiwidHJ1c3RlZF9hcHBzIjpbXSwiaXNHcmF2YXRhckVuYWJsZWQiOmZhbHNlLCJpbWFnZSI6IiIsImVtYWlsIjoiYWxpY2UtdGhlLWFkbWluQHRlc3QuY29tIiwiaWQiOiJhZG1pbiIsImF1dGhvcml6YXRpb25fZGVjaXNpb24iOiIiLCJhcHBfYXpmX2RvbWFpbiI6Im5yRldNSDhFRWVxQk5WWWJtN2M5ckEiLCJlaWRhc19wcm9maWxlIjp7fSwiYXR0cmlidXRlcyI6e30sInVzZXJuYW1lIjoiYWxpY2UiLCJ0eXBlIjoidXNlciIsImlhdCI6MTU4NzIwNjYzNiwiZXhwIjoyNTg3MjEwMjM2fQ.dP6TAXmNAIPRsEH5VjrNA-oySo6wDkhL-qfAnTtOByRGicCYTHxmAc39_53oRwkW_Y-gPz6CgWxZUgqHbPOU5e3nNq6fyPFIbYXDVfL_7_nTQ4x2PV38sxn0msSJ9l2O2YF_bKn-rRJXoAYJkXivRsvNSPbRFQZRgcOi4iMikJ5jOWMqrnOlI48ArCNn473Aw_HXX2GdyOPvou23LCripeTvBqaJf5Lrx6GxtJa9MG-u56968SaIuNqCC-eIfubUE66jEQVx79q3kiy3yLtfLxAXG6paqMbvNenQMS8qLB6HtBOaJgJaOe5oIEnVQvWvbuigFoSym2SahHizVVpq7g`,
		want:  []string{"Role1", "Role2", "Role3"},
	},
	{
		// tweaked from above token to have same orgs/roles as in
		// TestRolesWithSomeNames, decode on jwt.io
		token: `eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJvcmdhbml6YXRpb25zIjpbeyJpZCI6IjA5ZDQzOTVlLTkzMWMtNDc1Yy1hNzIzLWFmMzYyYjU2M2IxMSIsIm5hbWUiOiJPcmcyIiwiZGVzY3JpcHRpb24iOiJPcmcyIiwid2Vic2l0ZSI6bnVsbCwicm9sZXMiOlt7ImlkIjo0LCJuYW1lIjoicjQifSx7ImlkIjo1LCJuYW1lIjoiIn0seyJuYW1lIjoicjYifV19LHsiaWQiOiIzYWU2YTNhMy01MWY1LTQwMjUtOThjMy05Y2VhZWI1M2MwYjIiLCJuYW1lIjoiT3JnMSIsImRlc2NyaXB0aW9uIjoiT3JnMSIsIndlYnNpdGUiOm51bGwsInJvbGVzIjpbeyJpZCI6MSwibmFtZSI6InIxIn0seyJpZCI6MiwibmFtZSI6bnVsbH0seyJpZCI6M31dfV0sImRpc3BsYXlOYW1lIjoiIiwicm9sZXMiOlt7ImlkIjo3LCJuYW1lIjoicjcifSx7Im5hbWUiOiJyNiJ9LHt9XSwiYXBwX2lkIjoiMzBjY2Y5ODEtNjYzYy00NTNkLTg3ZWUtYzFjZTM2YjQ5N2FhIiwidHJ1c3RlZF9hcHBzIjpbXSwiaXNHcmF2YXRhckVuYWJsZWQiOmZhbHNlLCJpbWFnZSI6IiIsImVtYWlsIjoiYWxpY2UtdGhlLWFkbWluQHRlc3QuY29tIiwiaWQiOiJhZG1pbiIsImF1dGhvcml6YXRpb25fZGVjaXNpb24iOiIiLCJhcHBfYXpmX2RvbWFpbiI6Im5yRldNSDhFRWVxQk5WWWJtN2M5ckEiLCJlaWRhc19wcm9maWxlIjp7fSwiYXR0cmlidXRlcyI6e30sInVzZXJuYW1lIjoiYWxpY2UiLCJ0eXBlIjoidXNlciIsImlhdCI6MTU4NzIwNjYzNiwiZXhwIjoyNTg3MjEwMjM2fQ.GT2dVSpY8Y4rS2W-BuXMKOf2Puq2dn1Ed92U5nPZVkdgD1HmYKISoXeETe7JU45xVs9JcXqUggS9PL7m6b-tKeSVhSgb0gyMitfI1useEemrtk0EXg7_c22dLtiUXCxURk5eIwbID6qLX0KHOTCUXfpQGNh62sdunGGuNTnfMCHvXzFI220j5IKOmW4WLXwoICUsM2V005SLnqTRK18WaApNL_1kM79XNysy5Tqe9WIDp7yJcIpvNMIUBGFWbNQJbeUXR3hvoZGvO7yvO-fGOa5nJNlr59fT6LhWXHACzwa8XrXgBdbIv6vlbRjqdOzkqVStbAAiaDB5q1skjPrDBg`,
		want:  []string{"r1", "r4", "r6", "r7"},
	},
}

func TestExtractRolesFromJwt(t *testing.T) {
	for k, d := range extractRolesFromJwtFixtures {
		got := extractRolesFromJwt(t, d.token)
		if !reflect.DeepEqual(d.want, got) {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}
