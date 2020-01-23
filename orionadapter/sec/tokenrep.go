package token

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
)

// ClientHeader holds the parts of the IDS client header we're interested in.
type ClientHeader struct {
	SecurityToken struct {
		TokenValue string `json:"tokenValue"`
	} `json:"securityToken"`
}

func clientHeaderFromJSON(jsonData []byte) (*ClientHeader, error) {
	var data ClientHeader
	err := json.Unmarshal(jsonData, &data)
	return &data, err
}

func extractClientToken(idsHeaderValue string) (string, error) {
	jsonData := []byte(idsHeaderValue)
	header, err := clientHeaderFromJSON(jsonData)
	if err != nil {
		return "", err
	}
	return header.SecurityToken.TokenValue, nil
}

func jsonValueFromBase64(idsHeaderAttr string) (string, error) {
	decodedBytes, err := base64.StdEncoding.DecodeString(idsHeaderAttr)
	if err != nil {
		return "", err
	}
	return string(decodedBytes), nil
}

// ReadClientToken reads the IDS client token from the IDS header.
// The input idsHeaderAttr is the Mixer attribute that captured the value of
// the IDS header in the incoming HTTP request.
// The output token is the actual client token contained in the IDS header.
func ReadClientToken(idsHeaderAttr string) (token string, err error) {
	idsHeaderValue, err := jsonValueFromBase64(idsHeaderAttr)
	if err != nil {
		return "", headerDecodingError(err)
	}
	token, err = extractClientToken(idsHeaderValue)
	if err != nil {
		return "", tokenExtractionError(err)
	}
	return token, nil
}

// errors boilerplate

func headerDecodingError(cause error) error {
	return fmt.Errorf("error decoding IDS header: %v", cause)
}

func tokenExtractionError(cause error) error {
	return fmt.Errorf("error extracting client token from IDS header: %v",
		cause)
}
