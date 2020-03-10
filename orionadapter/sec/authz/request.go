package authz

import (
	"fmt"
	"strings"
)

const requestRoleTemplate = `
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:%s" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">%s</AttributeValue>
    </Attribute>
`

const requestTemplate = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false">
  <Attributes Category="urn:oasis:names:tc:xacml:1.0:subject-category:access-subject">
%s
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:resource">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:resource:resource-id" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">%s</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:thales:xacml:2.0:resource:path" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">%s</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:thales:xacml:2.0:resource:fiware-service" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">%s</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:action">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:action:action-id" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">%s</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:environment" />
</Request>
`

// Request holds the data to do an access control check with the
// AuthZ server.
type Request struct {
	Roles         []string
	ResourceID    string
	ResourcePath  string
	FiwareService string
	Action        string
}

func buildRoles(roles []string) string {
	var xmlBuf strings.Builder
	for _, r := range roles {
		roleAttribute := fmt.Sprintf(requestRoleTemplate, r, r)
		xmlBuf.WriteString(roleAttribute)
	}
	return xmlBuf.String()
}

func buildRequest(r *Request) string {
	rolesXML := buildRoles(r.Roles)
	return fmt.Sprintf(requestTemplate,
		rolesXML, r.ResourceID, r.ResourcePath, r.FiwareService, r.Action)
}
