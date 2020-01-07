package token

import (
	"encoding/json"
	"errors"
	"strings"

	ilog "istio.io/pkg/log"
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

// Poor man's implementation, to be scrapped.
// See: https://github.com/orchestracities/boost/issues/13
func parseIdsHeaderValue(rawValue string) (parsedValue string, err error) {
	if len(rawValue) == 0 {
		return "", nil
	}
	if rawValue[0] != '"' {
		// unquoted value, leave it be.
		return rawValue, nil
	}
	if len(rawValue) == 1 { // rawValue == `"`
		return "", errors.New("expecting quoted string")
	}
	quotedContent := rawValue[1 : len(rawValue)-1]    // chop off opening/closing "
	v := strings.ReplaceAll(quotedContent, `\/`, "/") // unescape /
	parsedValue = strings.ReplaceAll(v, `\"`, `"`)    // unescape "
	return parsedValue, nil
}

// ReadClientToken reads the IDS client token from the IDS header.
// The input idsHeaderAttr is the Mixer attribute that captured the value of
// the IDS header in the incoming HTTP request.
// The output token is the actual client token contained in the IDS header.
func ReadClientToken(idsHeaderAttr string) (token string, err error) {
	idsHeaderValue, err := parseIdsHeaderValue(idsHeaderAttr)
	if err != nil {
		ilog.Errorf("error parsing IDS header: %v", err)
		return "", err
	}
	token, err = extractClientToken(idsHeaderValue)
	if err != nil {
		ilog.Errorf("error extracting client token from IDS header: %v", err)
		return "", err
	}
	return token, nil
}
