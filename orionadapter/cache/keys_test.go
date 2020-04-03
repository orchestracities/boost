package cache

import (
	"encoding/base64"
	"fmt"
	"strings"
	"testing"

	"github.com/orchestracities/boost/orionadapter/sec/authz"
)

const clientJSONPayloadTemplate string = `{
	"securityToken": {
		"tokenValue": "%s"
	}
}`

func toB64(headerValue string) string {
	v := []byte(headerValue)
	return base64.StdEncoding.EncodeToString(v)
}

func clientJSONPayload(token string) string {
	headerValue := fmt.Sprintf(clientJSONPayloadTemplate, token)
	return toB64(headerValue)
}

func callParams(roles []string, path string, service string,
	action string) *authz.Request {
	return &authz.Request{
		Roles:         roles,
		ResourceID:    "b3a4a7d2-ce61-471f-b05d-fb82452ae686",
		ResourcePath:  path,
		FiwareService: service,
		Action:        action,
	}
}

func TestAuthZCallKeyContent(t *testing.T) {
	header := clientJSONPayload("my.fat.jwt")
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")

	got, _, err := authZCallKey(header, params)
	if err != nil {
		t.Errorf("%v", err)
	}

	for _, v := range []string{"my.fat.jwt", "r1", "/v2", "GET"} {
		if !strings.Contains(got, v) {
			t.Errorf("%v not in %v", v, got)
		}
	}
}

func TestAuthZCallKeyErrorOnNilCallParams(t *testing.T) {
	header := clientJSONPayload("my.fat.jwt")
	got, _, err := authZCallKey(header, nil)
	if err == nil {
		t.Errorf("want error; got: %v", got)
	}
}

func TestAuthZCallKeyErrorOnInvalidHeader(t *testing.T) {
	header := ""
	params := callParams([]string{"r1"}, "/v2", "svc", "GET")
	got, _, err := authZCallKey(header, params)
	if err == nil {
		t.Errorf("want error; got: %v", got)
	}
}
