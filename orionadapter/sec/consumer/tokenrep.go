package consumer

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"net/url"
	"path"
)

// Header holds the bits of the IDS consumer header we're interested in.
type Header struct {
	IssuerConnector string `json:"issuerConnector"`
	SecurityToken   struct {
		TokenValue string `json:"tokenValue"`
	} `json:"securityToken"`
}

func consumerHeaderFromJSON(jsonData []byte) (*Header, error) {
	var data Header
	err := json.Unmarshal(jsonData, &data)
	return &data, err
}

func jsonValueFromBase64(idsHeaderAttr string) (string, error) {
	decodedBytes, err := base64.StdEncoding.DecodeString(idsHeaderAttr)
	if err != nil {
		return "", err
	}
	return string(decodedBytes), nil
}

func extractHeader(idsHeaderAttr string) (*Header, error) {
	idsHeaderValue, err := jsonValueFromBase64(idsHeaderAttr)
	if err != nil {
		return nil, headerDecodingError(err)
	}

	jsonData := []byte(idsHeaderValue)
	header, err := consumerHeaderFromJSON(jsonData)
	if err != nil {
		return nil, headerParsingError(err)
	}

	return header, nil
}

// ReadToken reads the IDS consumer JWT from the IDS header.
// The input idsHeaderAttr is the Mixer attribute that captured the value of
// the IDS header in the incoming HTTP request.
// The output token is the actual consumer token contained in the IDS header.
func ReadToken(idsHeaderAttr string) (token string, err error) {
	header, err := extractHeader(idsHeaderAttr)
	if err != nil {
		return "", err
	}

	return header.SecurityToken.TokenValue, nil
}

// ReadIssuerConnectorID reads the issuer connector ID from the IDS header.
// The input idsHeaderAttr is the Mixer attribute that captured the value of
// the IDS header in the incoming HTTP request.
// The output ID is the last path component of the URL specified by the
// 'issuerConnector' element, e.g. if
//     "issuerConnector": "http://issu.er/connector/59a68243-dd96-4c8d-88a9-0f0e03e13b1b"
// the returned ID would be "59a68243-dd96-4c8d-88a9-0f0e03e13b1b".
// Return empty if there's no 'issuerConnector' element, it's empty or it's
// a URL but its last element is empty.
func ReadIssuerConnectorID(idsHeaderAttr string) (id string, err error) {
	header, err := extractHeader(idsHeaderAttr)
	if err != nil {
		return "", err
	}

	parsed, err := url.Parse(header.IssuerConnector)
	if err != nil {
		return "", issuerConnectorParsingError(err)
	}

	lastElement := path.Base(parsed.Path)
	if lastElement == "/" || lastElement == "." {
		return "", nil
	}
	return lastElement, nil
	// NOTE. Base corner cases.
	//   path.Base("/a/b/") == "b"
	//   path.Base("/")     == "/"
	//   path.Base("")      == "."
}

// errors boilerplate

func headerDecodingError(cause error) error {
	return fmt.Errorf("error decoding IDS header from Base64: %v", cause)
}

func headerParsingError(cause error) error {
	return fmt.Errorf("error parsing IDS header from JSON: %v", cause)
}

func issuerConnectorParsingError(cause error) error {
	return fmt.Errorf("error parsing issuerConnector URL from IDS header: %v",
		cause)
}
