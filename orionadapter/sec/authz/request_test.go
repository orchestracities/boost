package authz

import (
	"testing"
)

const expectedRequestXML = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false">
  <Attributes Category="urn:oasis:names:tc:xacml:1.0:subject-category:access-subject">

    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:role0" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role0</AttributeValue>
    </Attribute>

    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:role1" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role1</AttributeValue>
    </Attribute>

    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:role2" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role2</AttributeValue>
    </Attribute>

    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:role3" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role3</AttributeValue>
    </Attribute>

  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:resource">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:resource:resource-id" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">b3a4a7d2-ce61-471f-b05d-fb82452ae686</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:thales:xacml:2.0:resource:path" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">/v2</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:thales:xacml:2.0:resource:fiware-service" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">service</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:action">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:action:action-id" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">GET</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:environment" />
</Request>
`

func TestTypicalRequest(t *testing.T) {
	req := &Request{
		Roles:         []string{"role0", "role1", "role2", "role3"},
		ResourceID:    "b3a4a7d2-ce61-471f-b05d-fb82452ae686",
		ResourcePath:  "/v2",
		FiwareService: "service",
		Action:        "GET",
	}
	got := buildRequest(req)
	if expectedRequestXML != got {
		t.Errorf("got: %s", got)
	}
}
