package token

import (
	"fmt"
	"testing"
)

const clientHeaderValueTemplate string = `{
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

const rawClientHeaderValueTemplate string = `"{` +
	`\"@type\":\"ids:QueryMessage\",` +
	`\"id\":\"http:\/\/industrialdataspace.org\/queryMessage\/1a421b8c-3407-44a8-aeb9-253f145c869a\",` +
	`\"issued\":\"2019-10-25T15:35:34.589Z\",` +
	`\"modelVersion\":\"2.1.0\",` +
	`\"issuerConnector\":\"https:\/\/companyA.com\/connector\/59a68243-dd96-4c8d-88a9-0f0e03e13b1b\",` +
	`\"securityToken\":{` +
	`\"@type\":\"ids:DynamicAttributeToken\",` +
	`\"tokenFormat\":\"https:\/\/w3id.org\/idsa\/code\/tokenformat\/JWT\",` +
	`\"tokenValue\":\"%s\"` +
	`}` +
	`}"`

func clientHeaderValueWithToken(t string) string {
	return fmt.Sprintf(clientHeaderValueTemplate, t)
}

func rawClientHeaderValueWithToken(t string) string {
	return fmt.Sprintf(rawClientHeaderValueTemplate, t)
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
	header := clientHeaderValueWithToken(want)
	assertExtractClientToken(t, header, want)
}

func TestExtractClientTokenWithoutToken(t *testing.T) {
	want := ""
	header := clientHeaderValueWithToken(want)
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

func assertReadClientToken(t *testing.T, idsHeaderAttr string, want string) {
	if got, err := ReadClientToken(idsHeaderAttr); err != nil {
		t.Errorf("%s", err)
	} else if got != want {
		t.Errorf("wrong token. got: %s, want: %s", got, want)
	}
}

func TestReadClientTokenWithToken(t *testing.T) {
	want := "my.fat.jwt"
	header := rawClientHeaderValueWithToken(want)
	assertReadClientToken(t, header, want)
}

func TestReadClientTokenWithoutToken(t *testing.T) {
	want := ""
	header := clientHeaderValueWithToken(want)
	assertReadClientToken(t, header, want)
}

func TestReadClientTokenWithoutTokenValueField(t *testing.T) {
	want := ""
	header := `"{ \"securityToken\": {} }"`
	assertReadClientToken(t, header, want)
}

func TestReadClientTokenWithoutSecurityTokenField(t *testing.T) {
	want := ""
	header := `"{}"`
	assertReadClientToken(t, header, want)
}

func TestReadClientTokenWithEmptyHeader(t *testing.T) {
	header := ""
	if _, err := ReadClientToken(header); err == nil {
		t.Error("should've returned a JSON parsing error.")
	}
}

func TestReadClientTokenWithMalformedHeader(t *testing.T) {
	header := `"`
	if _, err := ReadClientToken(header); err == nil {
		t.Error("should've returned a quoted value parsing error.")
	}
}

func TestReadClientTokenWithEmptyQuotedValue(t *testing.T) {
	header := `""`
	if _, err := ReadClientToken(header); err == nil {
		t.Error("should've returned a JSON parsing error.")
	}
}

func TestReadClientTokenWithInvalidJSON(t *testing.T) {
	header := `"{"`
	if _, err := ReadClientToken(header); err == nil {
		t.Error("should've returned a JSON parsing error.")
	}
}
