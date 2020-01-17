package token

import (
	"encoding/base64"
	"fmt"
	"strings"
	"testing"
)

const serverHeaderJSONTemplate string = `{
	"@type": "ids:ResultMessage",
	"id": "http://industrialdataspace.org/resultMessage/1a421b8c-3407-44a8-aeb9-253f145c869a",
	"issued": "%s",
	"modelVersion": "2.1.0",
	"issuerConnector": "https://companyA.com/connector/59a68243-dd96-4c8d-88a9-0f0e03e13b1b",
	"securityToken": {
		"@type": "ids:DynamicAttributeToken",
		"tokenFormat": "https://w3id.org/idsa/code/tokenformat/JWT",
		"tokenValue": "%s"
	}
}`

func TestBuildServerHeader(t *testing.T) {
	token := "my.fat.jwt"
	b64rep, err := BuildServerHeader(serverHeaderJSONTemplate, token)
	if err != nil {
		t.Errorf("can't build header: %v", err)
	}
	jsonBytes, err := base64.StdEncoding.DecodeString(b64rep)
	if err != nil {
		t.Errorf("didn't base-64 encode properly: %v", err)
	}

	jsonStr := string(jsonBytes)
	if !strings.Contains(jsonStr, token) {
		t.Errorf("didn't replace token: %s", jsonStr)
	}
	if strings.Contains(jsonStr, "%s") {
		t.Errorf("didn't replace issued field: %s", jsonStr)
	}
}

func TestBuildServerHeaderFromEmptyTemplate(t *testing.T) {
	token := "my.fat.jwt"
	b64rep, err := BuildServerHeader("{}", token)
	if err != nil {
		t.Errorf("can't build header: %v", err)
	}
	jsonBytes, err := base64.StdEncoding.DecodeString(b64rep)
	if err != nil {
		t.Errorf("didn't base-64 encode properly: %v", err)
	}

	jsonStr := string(jsonBytes)
	if !strings.Contains(jsonStr, token) {
		t.Errorf("didn't output token: %s", jsonStr)
	}
	if strings.Contains(jsonStr, "%s") {
		t.Errorf("didn't output issued field: %s", jsonStr)
	}
}

var buildServerHeaderWithMalformedTemplate = []struct {
	template string
}{
	{""}, {"{"}, {"}"}, {"junk"}, {"\n}"},
}

func TestBuildServerHeaderWithMalformedTemplate(t *testing.T) {
	token := "my.fat.jwt"
	for _, k := range buildServerHeaderWithMalformedTemplate {
		b64rep, err := BuildServerHeader(k.template, token)
		if err == nil {
			t.Errorf("shouldn't build header from malformed template: %s\n%s",
				k.template, b64rep)
		}
	}
}

func TestServerHeaderSerializationErrorBuilding(t *testing.T) {
	cause := fmt.Errorf("whoa")
	if err := serverHeaderSerializationError(cause); err == nil {
		t.Errorf("should've build an error")
	}
}
