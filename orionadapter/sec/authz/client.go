package authz

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	"github.com/orchestracities/boost/orionadapter/sec/authz/xacml"
)

// Client talks to an AuthZ server. Never instantiate one directly,
// rather use NewClient.
type Client struct {
	base *url.URL
	conn *http.Client
}

// NewClient instantiates an AuthZ client to connect to the server endpoint
// specified by the URL param.
func NewClient(serverURL string) (*Client, error) {
	endpoint, err := parseBaseURL(serverURL)
	if err != nil {
		return nil, err
	}
	return &Client{
		base: endpoint,
		conn: &http.Client{},
	}, nil
}

// Authorize asks the AuthZ server to authorize the given request.
// Returns true just in case the server gave the green light, false for
// permission denied, and error if some technical problem happened.
func (c *Client) Authorize(r *xacml.Request) (bool, error) {
	requestURL := buildRequestURL(c.base, r.KeyRock.AppAzfDomain)
	requestBody := strings.NewReader(r.ToXML())
	resp, err := c.conn.Post(requestURL, "application/xml", requestBody)
	if err != nil {
		return false, err
	}
	defer resp.Body.Close()

	authzResponse, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return false, err
	}
	return xacml.IsPermitDecision(authzResponse)
}

// TODO: proper handling of HTTP response errors.
// Do something similar to dapsclient.handleResponse and factor out common code.

func parseBaseURL(rawURL string) (*url.URL, error) {
	parsed, err := url.Parse(rawURL)
	if err != nil {
		return nil, err
	}
	if parsed.Scheme == "" || parsed.Host == "" {
		return nil, invalidServerURL(rawURL)
	}
	return parsed, nil
}

func buildRequestURL(base *url.URL, appDomainID string) string {
	escapedID := url.PathEscape(appDomainID)
	return fmt.Sprintf("%s://%s/authzforce-ce/domains/%s/pdp",
		base.Scheme, base.Host, escapedID)
}

// errors boilerplate

func invalidServerURL(rawURL string) error {
	return fmt.Errorf("invalid AuthZ server base URL: %s", rawURL)
}
