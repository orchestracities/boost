package daps

import (
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
)

type baseURL struct {
	scheme string
	host   string // host or host:port (same as net.url.URL.Host)
}

func (u *baseURL) join(rest string) string {
	if len(rest) == 0 || rest[0] != '/' {
		rest = "/" + rest
	}
	return fmt.Sprintf("%s://%s%s", u.scheme, u.host, rest)
}

// Client to talk to a DAPS server. Never instantiate one directly,
// rather use NewClient.
type Client struct {
	base *baseURL
	conn *http.Client
}

// NewClient builds an HTTP client to connect to the specified DAPS
// host using mTLS. host can be either a plain host name or a host name
// with a port number as in e.g. "whoopsie.dapsie.org:4433".
// clientCert and clientPvtKey are, respectively, the certificate and
// associated private key the client should use to authenticate with DAPS
// whereas serverCert is the DAPS server certificate the client should
// use to authenticate the server. All of them are supposed to be in PEM
// format.
func NewClient(host string, clientPvtKey string, clientCert string,
	serverCert string) (*Client, error) {

	clCert, err := tls.X509KeyPair([]byte(clientCert), []byte(clientPvtKey))
	if err != nil {
		return &Client{}, err
	}

	svrCerts := x509.NewCertPool()
	if !svrCerts.AppendCertsFromPEM([]byte(serverCert)) {
		return &Client{}, serverCertDecodingError()
	}

	return &Client{
		base: &baseURL{
			scheme: "https",
			host:   host,
		},
		conn: &http.Client{
			Transport: &http.Transport{
				TLSClientConfig: &tls.Config{
					RootCAs:      svrCerts,
					Certificates: []tls.Certificate{clCert},
				},
			},
		},
	}, nil
}

func checkSuccess(r *http.Response, err error) error {
	if err != nil {
		return err
	}
	if 400 <= r.StatusCode {
		return httpError(r.Status)
	}
	if 200 <= r.StatusCode && r.StatusCode <= 204 {
		return nil
	}
	// 3xx should be handled by http client under the bonnet so we
	// shouldn't get to see them. Any other code (e.g. 100, 205,...)
	// we can't handle we reject for now.
	return unexpectedHTTPError(r.Status)
}

func checkHasContent(r *http.Response, err error) error {
	if err != nil {
		return err
	}
	if r.ContentLength == 0 {
		return noContentError(r.Status)
	}
	return nil
}

// ResponseBody holds any data returned in the HTTP response body.
type ResponseBody []byte

// IsEmpty tells if ResponseBody has any content.
func (b ResponseBody) IsEmpty() bool {
	return len(b) == 0
}

// AsString converts ResponseBody to string.
func (b ResponseBody) AsString() string {
	return string(b)
}

// AsJSON converts ResponseBody to JSON using the provided data structure.
func (b ResponseBody) AsJSON(out interface{}) error {
	return json.Unmarshal(b, out)
}

// AsJSONMap converts ResponseBody to a generic JSON map.
func (b ResponseBody) AsJSONMap() (map[string]interface{}, error) {
	var out map[string]interface{}
	if err := json.Unmarshal(b, &out); err != nil {
		return nil, err
	}
	return out, nil
}

func extractContent(r *http.Response, err error) (ResponseBody, error) {
	if err != nil {
		return nil, err
	}
	defer r.Body.Close()
	return ioutil.ReadAll(r.Body)
}

func handleResponse(ensureContent bool,
	r *http.Response, err error) (ResponseBody, error) {
	err = checkSuccess(r, err)
	if ensureContent {
		err = checkHasContent(r, err)
	}
	return extractContent(r, err)
}

// PostForm issues a POST to the specified URL path with data's keys and
// values URL-encoded as the request body.
//
// If the server replies with a success code, the response body gets
// read entirely in memory and returned. The returned data buffer may
// be empty if the server returned no content. If you'd like to return
// an error in this case (e.g. you expect content), then set ensureResponseBody
// to true.
//
// If the server replies with an error or a connection error occurs,
// you get back that error and a nil ResponseBody.
func (c *Client) PostForm(urlPath string, data url.Values,
	ensureResponseBody bool) (ResponseBody, error) {
	url := c.base.join(urlPath)
	r, err := c.conn.PostForm(url, data)
	return handleResponse(ensureResponseBody, r, err)
}

// errors boilerplate

func serverCertDecodingError() error {
	return fmt.Errorf("can't decode server certificate")
}

func httpError(statusLine string) error {
	return fmt.Errorf("http error: %s", statusLine)
}

func unexpectedHTTPError(statusLine string) error {
	return fmt.Errorf("unexpected http response: %s", statusLine)
}

func noContentError(statusLine string) error {
	return fmt.Errorf("server returned no content: %s", statusLine)
}
