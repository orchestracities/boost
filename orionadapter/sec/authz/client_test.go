package authz

import (
	"net/url"
	"testing"
)

var parseBaseURLErrorFixtures = []string{
	"", "host.com", "http:///some/path", "some/rel/path", "h@ : / j u n k !!",
	"http://weird host/some/path",
}

func TestParseBaseURLError(t *testing.T) {
	for k, rawURL := range parseBaseURLErrorFixtures {
		if got, err := parseBaseURL(rawURL); err == nil {
			t.Errorf("[%v] want: error; got: %v", k, got)
		}
	}
}

var parseBaseURLFixtures = []string{
	"http://host.com", "http://host/", "http://host/path",
	"http://host.com:8080", "http://host:8080/", "http://host:8080/path",
	"https://host.com:8080", "https://host:8080/", "https://host:8080/path",
}

func TestParseBaseURL(t *testing.T) {
	for k, rawURL := range parseBaseURLFixtures {
		got, err := parseBaseURL(rawURL)
		if err == nil {
			if got.String() != rawURL {
				t.Errorf("[%v] want: %v; got: %+v", k, rawURL, got)
			}
		} else {
			t.Errorf("[%v] unexpected error: %v", k, err)
		}
	}
}

var buildRequestURLFixtures = []struct {
	serverURL string
	domainID  string
	want      string
}{
	{
		serverURL: "http://authzforceingress.appstorecontainerns.46.17.108.63.xip.io/",
		domainID:  "wCIwcYFkEeqBNVYbm7c9rA",
		want:      "http://authzforceingress.appstorecontainerns.46.17.108.63.xip.io/authzforce-ce/domains/wCIwcYFkEeqBNVYbm7c9rA/pdp",
	},
	{
		serverURL: "https://host.me:4430",
		domainID:  "escape / this",
		want:      "https://host.me:4430/authzforce-ce/domains/escape%20%2F%20this/pdp",
	},
	{
		serverURL: "https://host.me:4430/",
		domainID:  "escape / this",
		want:      "https://host.me:4430/authzforce-ce/domains/escape%20%2F%20this/pdp",
	},
	{
		serverURL: "https://host.me:4430/ignore/me",
		domainID:  "dom-pedro",
		want:      "https://host.me:4430/authzforce-ce/domains/dom-pedro/pdp",
	},
}

func TestBuildRequestURLFixtures(t *testing.T) {
	for k, d := range buildRequestURLFixtures {
		base, _ := url.Parse(d.serverURL)
		got := buildRequestURL(base, d.domainID)
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}
func TestNewClientError(t *testing.T) {
	serverURL := "http://weird host"
	if got, err := NewClient(serverURL); err == nil {
		t.Errorf("want error; got: %+v", got)
	}
}
