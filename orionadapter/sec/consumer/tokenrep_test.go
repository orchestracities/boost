package consumer

import (
	"encoding/base64"
	"fmt"
	"testing"
)

const clientJSONPayloadTemplate string = `{
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

func toB64(headerValue string) string {
	v := []byte(headerValue)
	return base64.StdEncoding.EncodeToString(v)
}

func clientJSONPayload(token string) string {
	return fmt.Sprintf(clientJSONPayloadTemplate, token)
}

func assertReadToken(t *testing.T, jsonPayload string, want string) {
	idsHeaderAttr := toB64(jsonPayload)
	if got, err := ReadToken(idsHeaderAttr); err != nil {
		t.Errorf("%s", err)
	} else if got != want {
		t.Errorf("wrong token. got: %s, want: %s", got, want)
	}
}

func TestReadConsumerHeaderWithToken(t *testing.T) {
	want := "my.fat.jwt"
	header := clientJSONPayload(want)
	assertReadToken(t, header, want)
}

var readConsumerHeaderWithoutToken = []struct {
	jsonValue string
}{
	{clientJSONPayload("")},
	{`{"securityToken": {"tokenValue": null}}`},
}

func TestReadConsumerHeaderWithoutToken(t *testing.T) {
	want := ""
	for _, k := range readConsumerHeaderWithoutToken {
		assertReadToken(t, k.jsonValue, want)
	}
}

var readConsumerHeaderWithoutTokenValueField = []struct {
	jsonValue string
}{
	{`{ "securityToken": {} }`},
	{`{ "securityToken": { "@type": "ids:DynamicAttributeToken" } }`},
	{
		`{
			"securityToken": {
				"@type": "ids:DynamicAttributeToken",
				"tokenFormat": "https://w3id.org/idsa/code/tokenformat/JWT"
			}
		}`,
	},
	{
		`{
			"@type": "ids:QueryMessage",
			"id": "http://industrialdataspace.org/queryMessage/1a421b8c-3407-44a8-aeb9-253f145c869a",
			"issued": "2019-10-25T15:35:34.589Z",
			"modelVersion": "2.1.0",
			"issuerConnector": "https://companyA.com/connector/59a68243-dd96-4c8d-88a9-0f0e03e13b1b",
			"securityToken": {
				"@type": "ids:DynamicAttributeToken",
				"tokenFormat": "https://w3id.org/idsa/code/tokenformat/JWT"
			}
		}`,
	},
}

func TestReadConsumerHeaderWithoutTokenValueField(t *testing.T) {
	want := ""
	for _, k := range readConsumerHeaderWithoutTokenValueField {
		assertReadToken(t, k.jsonValue, want)
	}
}

var readConsumerHeaderWithoutSecurityTokenField = []struct {
	jsonValue string
}{
	{`{}`},
	{`{ "securityToken": null }`},
	{`{ "@type": "ids:QueryMessage" }`},
	{
		`{
			"@type": "ids:QueryMessage",
			"id": "http://industrialdataspace.org/queryMessage/1a421b8c-3407-44a8-aeb9-253f145c869a"
		}`,
	},
	{
		`{
			"@type": "ids:QueryMessage",
			"id": "http://industrialdataspace.org/queryMessage/1a421b8c-3407-44a8-aeb9-253f145c869a",
			"issued": "2019-10-25T15:35:34.589Z",
			"modelVersion": "2.1.0",
			"issuerConnector": "https://companyA.com/connector/59a68243-dd96-4c8d-88a9-0f0e03e13b1b"
		}`,
	},
}

func TestReadConsumerHeaderWithoutSecurityTokenField(t *testing.T) {
	want := ""
	for _, k := range readConsumerHeaderWithoutSecurityTokenField {
		assertReadToken(t, k.jsonValue, want)
	}
}

func TestReadEmptyConsumerHeader(t *testing.T) {
	headerValue := ""
	if _, err := ReadToken(headerValue); err == nil {
		t.Error("should've returned a JSON parsing error.")
	}
}

var readMalformedConsumerHeaderValue = []struct {
	headerValue string
}{
	{`"`}, {`""`}, {`not base 64`}, {`{}`}, {clientJSONPayload("t.k.n")},
}

func TestReadMalformedConsumerHeaderValue(t *testing.T) {
	for _, k := range readMalformedConsumerHeaderValue {
		if _, err := ReadToken(k.headerValue); err == nil {
			t.Error("should've returned a Base 64 parsing error.")
		}
	}
}

var readConsumerHeaderWithInvalidJSON = []struct {
	jsonValue string
}{
	{`{`}, {`{{`}, {`{{}}`}, {`}`}, {`{in:valid}`},
}

func TestReadConsumerHeaderWithInvalidJSON(t *testing.T) {
	for _, k := range readConsumerHeaderWithInvalidJSON {
		headerValue := toB64(k.jsonValue)
		if _, err := ReadToken(headerValue); err == nil {
			t.Error("should've returned a JSON parsing error.")
		}
	}
}
