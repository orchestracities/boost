package jwt

import (
	"testing"
)

// real-world DAPS token.
/* Payload:
{
  "ids_attributes": {
    "security_profile": {
      "audit_logging": 2
    },
    "membership": true,
    "ids-uri": "http://some-uri",
    "transport_certs_sha258": "bacb879575730bb083f283fd5b67a8cb896944d1be28c7b32117cfc757c81e96"
  },
  "scopes": [
    "ids_connector"
  ],
  "aud": "IDS_Connector",
  "iss": "https://daps.aisec.fraunhofer.de",
  "sub": "C=DE,O=FIWARE,OU=CTOIDSA,CN=4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
  "nbf": 1588163283,
  "exp": 1588166883
}
*/
var dapsJWTExample = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImRlZmF1bHQifQ.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7ImF1ZGl0X2xvZ2dpbmciOjJ9LCJtZW1iZXJzaGlwIjp0cnVlLCJpZHMtdXJpIjoiaHR0cDovL3NvbWUtdXJpIiwidHJhbnNwb3J0X2NlcnRzX3NoYTI1OCI6ImJhY2I4Nzk1NzU3MzBiYjA4M2YyODNmZDViNjdhOGNiODk2OTQ0ZDFiZTI4YzdiMzIxMTdjZmM3NTdjODFlOTYifSwic2NvcGVzIjpbImlkc19jb25uZWN0b3IiXSwiYXVkIjoiSURTX0Nvbm5lY3RvciIsImlzcyI6Imh0dHBzOi8vZGFwcy5haXNlYy5mcmF1bmhvZmVyLmRlIiwic3ViIjoiQz1ERSxPPUZJV0FSRSxPVT1DVE9JRFNBLENOPTRlMTZmMDA3LWQ5NTktNGViMi1iNDdkLTc4ZGQwYzRlYWIwZSIsIm5iZiI6MTU4ODE2MzI4MywiZXhwIjoxNTg4MTY2ODgzfQ.bcjXqapTZs7VRjOatCoaPH04_lTRvulzvYiPZcOpO1k7y8ZK1JggZMM955_7p-cDb8iH1fsDegk_tRG12Ex58PQ_S4qRJZv3kYQTWtSx6boyBvnoOAlw61LqPDuJaE1SncxOFAqFi8h0cotxnkmRUqnwwq8WXex9e4UwONZ3PNY_wny0iwbwqD2ZOKi380kLdYn8Nw-nISpQy82W9L-7cKf8xQvU5fD3EVjSb9ptzCdN8dq0PdVEPIcRNaVOxwQril75UgxX9AVFRyym9vPCHDh2AdAmD3Wb8SwUR7bnwLI6hAy2x1W5FsDUI50YFVRK7_A93UQqGmQp-UxbL5FStQ"

func TestDAPSExample(t *testing.T) {
	payload := FromRaw(dapsJWTExample)

	got := payload.SecProfileAuditLogging()
	want := "2"
	if got != want {
		t.Errorf("SecProfileAuditLogging. want: %v; got: %v", want, got)
	}

	got = payload.Membership()
	want = "true"
	if got != want {
		t.Errorf("Membership. want: %v; got: %v", want, got)
	}

	got = payload.Issuer()
	want = "https://daps.aisec.fraunhofer.de"
	if got != want {
		t.Errorf("Issuer. want: %v; got: %v", want, got)
	}

	got = payload.Subject()
	want = "C=DE,O=FIWARE,OU=CTOIDSA,CN=4e16f007-d959-4eb2-b47d-78dd0c4eab0e"
	if got != want {
		t.Errorf("Subject. want: %v; got: %v", want, got)
	}

	got = payload.SubjectCommonName()
	want = "4e16f007-d959-4eb2-b47d-78dd0c4eab0e"
	if got != want {
		t.Errorf("SubjectCommonName. want: %v; got: %v", want, got)
	}
}
