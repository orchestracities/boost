package authz

import (
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/orchestracities/boost/orionadapter/sec/authz/xacml"
)

// Client talks to an AuthZ server. Never instantiate one directly,
// rather use NewClient.
type Client struct {
	base string
	conn *http.Client
}

// NewClient instantiates an AuthZ client to connect to the server endpoint
// specified by the URL param.
func NewClient(serverURL string) *Client {
	return &Client{
		base: serverURL,
		conn: &http.Client{},
	}
}

// Authorize asks the AuthZ server to authorize the given request.
// Returns true just in case the server gave the green light, false for
// permission denied, and error if some technical problem happened.
func (c *Client) Authorize(r *Request) (bool, error) {
	authzRequest := buildRequest(r)

	requestBody := strings.NewReader(authzRequest)
	resp, err := c.conn.Post(c.base, "application/xml", requestBody)
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
