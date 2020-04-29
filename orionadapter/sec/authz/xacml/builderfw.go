package xacml

// Specialized constructors for IDS XACML request.
// see builder for generic constructors.

func accessSubject() xNode {
	return attrCategory(
		"urn:oasis:names:tc:xacml:1.0:subject-category:access-subject")
}

func fwKeyRockRole(roles []string) xNode {
	id := "urn:oasis:names:tc:xacml:2.0:subject:fw_keyrock_role"
	return attr(id).children(listOfStringAttr(roles)...)
}

func idsSecurityProfileAuditLogging() xNode {
	id := "urn:oasis:names:tc:xacml:2.0:subject:ids_security_profile_audit_logging"
	return attr(id).children(stringAttr("2"))
}

func idsSecurityProfilePseudoSecondElement() xNode {
	id := "urn:oasis:names:tc:xacml:2.0:subject:ids_security_profile_pseudo_second_element"
	return attr(id).children(
		stringAttr("ids_security_profile_pseudo_second_element"))
}

func idsMembership() xNode {
	id := "urn:oasis:names:tc:xacml:2.0:subject:ids_membership"
	return attr(id).children(boolAttr(true))
}

func idsScopes(scopes []string) xNode {
	id := "urn:oasis:names:tc:xacml:2.0:subject:ids_scope"
	return attr(id).children(listOfStringAttr(scopes)...)
}

func idsDapsIss(uri string) xNode {
	id := "urn:oasis:names:tc:xacml:2.0:subject:ids_daps_iss"
	return attr(id).children(uriAttr(uri))
}

func idsDapsSubConnectorCN(connectorID string) xNode {
	id := "urn:oasis:names:tc:xacml:2.0:subject:ids_daps_sub_connectorCN"
	return attr(id).children(stringAttr(connectorID))
}

func resource() xNode {
	return attrCategory(
		"urn:oasis:names:tc:xacml:3.0:attribute-category:resource")
}

func resourceID(rid string) xNode {
	id := "urn:oasis:names:tc:xacml:1.0:resource:resource-id"
	return attr(id).children(stringAttr(rid))
}

func requestPath(path string) xNode {
	id := "urn:thales:xacml:2.0:resource:request_path"
	return attr(id).children(stringAttr(path))
}

func requestFiwareService(service string) xNode {
	id := "urn:thales:xacml:2.0:resource:request_fiware_service"
	return attr(id).children(stringAttr(service))
}

func action() xNode {
	return attrCategory(
		"urn:oasis:names:tc:xacml:3.0:attribute-category:action")
}

func requestVerb(verb string) xNode {
	id := "urn:oasis:names:tc:xacml:1.0:action:fw_request_verb"
	return attr(id).children(stringAttr(verb))
}

func environment() xNode {
	return attrCategory(
		"urn:oasis:names:tc:xacml:3.0:attribute-category:environment")
}
