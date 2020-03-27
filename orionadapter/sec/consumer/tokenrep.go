package consumer

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
)

// Header holds the bits of the IDS consumer header we're interested in.
type Header struct {
	SecurityToken struct {
		TokenValue string `json:"tokenValue"`
	} `json:"securityToken"`
}

func consumerHeaderFromJSON(jsonData []byte) (*Header, error) {
	var data Header
	err := json.Unmarshal(jsonData, &data)
	return &data, err
}

func extractConsumerToken(idsHeaderValue string) (string, error) {
	jsonData := []byte(idsHeaderValue)
	header, err := consumerHeaderFromJSON(jsonData)
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

// ReadToken reads the IDS consumer JWT from the IDS header.
// The input idsHeaderAttr is the Mixer attribute that captured the value of
// the IDS header in the incoming HTTP request.
// The output token is the actual consumer token contained in the IDS header.
func ReadToken(idsHeaderAttr string) (token string, err error) {
	idsHeaderValue, err := jsonValueFromBase64(idsHeaderAttr)
	if err != nil {
		return "", headerDecodingError(err)
	}
	token, err = extractConsumerToken(idsHeaderValue)
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
	return fmt.Errorf("error extracting consumer token from IDS header: %v",
		cause)
}
