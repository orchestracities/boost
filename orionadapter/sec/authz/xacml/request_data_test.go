package xacml

var fullyFledgedRequest = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false">
  <Attributes Category="urn:oasis:names:tc:xacml:1.0:subject-category:access-subject">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:fw_keyrock_role" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role1</AttributeValue>
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role3</AttributeValue>
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role4</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_security_profile_audit_logging" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">2</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_security_profile_pseudo_second_element" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">ids_security_profile_pseudo_second_element</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_membership" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#boolean">true</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_scope" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">ids_connector</AttributeValue>
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">ids_scope_pseudo_second_element</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_daps_iss" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#anyURI">https://daps.aisec.fraunhofer.de</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_daps_sub_connectorCN" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">4e16f007-d959-4eb2-b47d-78dd0c4eab0e</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:resource">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:resource:resource-id" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">7bc2b735-7fdd-40ea-8cb2-acae9b241ca7</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:thales:xacml:2.0:resource:request_path" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">/v2/</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:thales:xacml:2.0:resource:request_fiware_service" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">service</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:action">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:action:fw_request_verb" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">GET</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:environment"/>
</Request>
`

var requestWithMissingIssuerAndService = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false">
  <Attributes Category="urn:oasis:names:tc:xacml:1.0:subject-category:access-subject">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:fw_keyrock_role" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role1</AttributeValue>
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role3</AttributeValue>
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">role4</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_security_profile_audit_logging" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">2</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_security_profile_pseudo_second_element" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">ids_security_profile_pseudo_second_element</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_membership" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#boolean">true</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_scope" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">ids_connector</AttributeValue>
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">ids_scope_pseudo_second_element</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_daps_sub_connectorCN" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">4e16f007-d959-4eb2-b47d-78dd0c4eab0e</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:resource">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:resource:resource-id" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">7bc2b735-7fdd-40ea-8cb2-acae9b241ca7</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:thales:xacml:2.0:resource:request_path" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">/v2/</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:action">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:1.0:action:fw_request_verb" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">GET</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:environment"/>
</Request>
`

var minimalRequest = `<?xml version="1.0" encoding="UTF-8"?>
<Request xmlns="urn:oasis:names:tc:xacml:3.0:core:schema:wd-17" CombinedDecision="false" ReturnPolicyIdList="false">
  <Attributes Category="urn:oasis:names:tc:xacml:1.0:subject-category:access-subject">
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_security_profile_audit_logging" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">2</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_security_profile_pseudo_second_element" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#string">ids_security_profile_pseudo_second_element</AttributeValue>
    </Attribute>
    <Attribute AttributeId="urn:oasis:names:tc:xacml:2.0:subject:ids_membership" IncludeInResult="false">
      <AttributeValue DataType="http://www.w3.org/2001/XMLSchema#boolean">true</AttributeValue>
    </Attribute>
  </Attributes>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:resource"/>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:action"/>
  <Attributes Category="urn:oasis:names:tc:xacml:3.0:attribute-category:environment"/>
</Request>
`
