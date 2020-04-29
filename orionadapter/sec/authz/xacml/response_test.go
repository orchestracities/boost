package xacml

import (
	"testing"
)

const typicalPermitResponse = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<ns4:Response
    xmlns="http://authzforce.github.io/rest-api-model/xmlns/authz/5"
    xmlns:ns2="http://www.w3.org/2005/Atom"
    xmlns:ns3="http://authzforce.github.io/pap-dao-flat-file/xmlns/properties/3.6"
    xmlns:ns4="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17"
    xmlns:ns5="http://authzforce.github.io/core/xmlns/pdp/6.0">
    <ns4:Result>
        <ns4:Decision>Permit</ns4:Decision>
    </ns4:Result>
</ns4:Response>
`

const permitResponseWithoutNamespaces = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Response>
    <Result>
        <Decision>Permit</Decision>
    </Result>
</Response>
`

const unformattedPermitFlag = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<ns4:Response
    xmlns="http://authzforce.github.io/rest-api-model/xmlns/authz/5"
    xmlns:ns2="http://www.w3.org/2005/Atom"
    xmlns:ns3="http://authzforce.github.io/pap-dao-flat-file/xmlns/properties/3.6"
    xmlns:ns4="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17"
    xmlns:ns5="http://authzforce.github.io/core/xmlns/pdp/6.0">
    <ns4:Result>
        <ns4:Decision> pErmiT	</ns4:Decision>
    </ns4:Result>
</ns4:Response>
`

var permitResponses = []string{
	typicalPermitResponse, permitResponseWithoutNamespaces,
	unformattedPermitFlag,
}

func TestPermitResponse(t *testing.T) {
	for k, response := range permitResponses {
		got, err := IsPermitDecision([]byte(response))
		if err != nil {
			t.Errorf("[%v] got error: %v", k, err)
		}
		if !got {
			t.Errorf("[%v] got false", k)
		}
	}
}

const typicalDenyResponse = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<ns4:Response xmlns="http://authzforce.github.io/rest-api-model/xmlns/authz/5"
			  xmlns:ns2="http://www.w3.org/2005/Atom"
			  xmlns:ns3="http://authzforce.github.io/pap-dao-flat-file/xmlns/properties/3.6"
			  xmlns:ns4="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17"
			  xmlns:ns5="http://authzforce.github.io/core/xmlns/pdp/6.0">
	<ns4:Result>
		<ns4:Decision>Deny</ns4:Decision>
	</ns4:Result>
</ns4:Response>`

const denyResponseWithoutNamespaces = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Response>
	<Result>
		<Decision>Deny</Decision>
	</Result>
</Response>`

var denyResponses = []string{
	typicalDenyResponse,
	denyResponseWithoutNamespaces,
}

func TestDenyResponse(t *testing.T) {
	for k, response := range denyResponses {
		got, err := IsPermitDecision([]byte(response))
		if err != nil {
			t.Errorf("[%v] got error: %v", k, err)
		}
		if got {
			t.Errorf("[%v] got true", k)
		}
	}
}

func TestInvalidXML(t *testing.T) {
	response := `<?xml>`
	got, err := IsPermitDecision([]byte(response))
	if err == nil {
		t.Errorf("want error but got: %v", got)
	}
	if got {
		t.Errorf("got true")
	}
}

func TestUnknownXML(t *testing.T) {
	response := `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
		<Error>computer says no</Error>
	`
	got, err := IsPermitDecision([]byte(response))
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if got {
		t.Errorf("got true")
	}
}
