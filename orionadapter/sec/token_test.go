package token

import (
	"fmt"
	"testing"
)

var clientHeaderTemplate string = `{
	"@type": "ids:QueryMessage",
	"id": "http://industrialdataspace.org/queryMessage/1a421b8c-3407-44a8-aeb9-253f145c869a",
	"issued": "2019-10-25T15:35:34.589Z",
	"modelVersion": "2.1.0",
	"issuerConnector": "https://companyA.com/connector/59a68243-dd96-4c8d-88a9-0f0e03e13b1b",
	"securityToken": {
		"@type": "ids:DynamicAttributeToken",
		"tokenFormat": "https://w3id.org/idsa/code/tokenformat/JWT",
		"tokenValue": "%s"
	}
}`

func clientHeaderWithToken(t string) string {
	return fmt.Sprintf(clientHeaderTemplate, t)
}

func assertExtractClientToken(t *testing.T, header string, want string) {
	if got, err := extractClientToken(header); err != nil {
		t.Errorf("%s", err)
	} else if got != want {
		t.Errorf("wrong token. got: %s, want: %s", got, want)
	}
}

func TestExtractClientTokenWithToken(t *testing.T) {
	want := "my.fat.jwt"
	header := clientHeaderWithToken(want)
	assertExtractClientToken(t, header, want)
}

func TestExtractClientTokenWithoutToken(t *testing.T) {
	want := ""
	header := clientHeaderWithToken(want)
	assertExtractClientToken(t, header, want)
}

func TestExtractClientTokenWithoutTokenValueField(t *testing.T) {
	want := ""
	header := `{ "securityToken": {} }`
	assertExtractClientToken(t, header, want)
}

func TestExtractClientTokenWithoutSecurityTokenField(t *testing.T) {
	want := ""
	header := "{}"
	assertExtractClientToken(t, header, want)
}

func TestExtractClientTokenWithEmptyHeader(t *testing.T) {
	header := ""
	if _, err := extractClientToken(header); err == nil {
		t.Error("should've returned a JSON parsing error.")
	}
}

func TestExtractClientTokenWithInvalidJSON(t *testing.T) {
	header := "{"
	if _, err := extractClientToken(header); err == nil {
		t.Error("should've returned a JSON parsing error.")
	}
}
