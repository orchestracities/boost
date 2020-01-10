#!/usr/bin/env bash

# Build an IDSA token header value from a template and put it on stdout.
# Pass the JWT token as the first argument to this script or call the
# script with no arguments to build a header value with an empty token
# in it.

set -e

TOKENARG=$1

base64 <<EOT
{
	"@type": "ids:QueryMessage",
	"id": "http://industrialdataspace.org/queryMessage/1a421b8c-3407-44a8-aeb9-253f145c869a",
	"issued": "2019-10-25T15:35:34.589Z",
	"modelVersion": "2.1.0",
	"issuerConnector": "https://companyA.com/connector/59a68243-dd96-4c8d-88a9-0f0e03e13b1b",
	"securityToken": {
		"@type": "ids:DynamicAttributeToken",
		"tokenFormat": "https://w3id.org/idsa/code/tokenformat/JWT",
		"tokenValue": "${TOKENARG}"
	}
}
EOT
