package token

import (
	"encoding/json"
)

// ClientHeader holds the content of the IDS client header we're interested in.
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
