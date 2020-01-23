package token

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"github.com/google/uuid"
	"time"
)

// ServerHeader holds the IDS server header we generate.
type ServerHeader struct {
	Type            string `json:"@type"`
	ID              string `json:"id"`
	Issued          string `json:"issued"`
	ModelVersion    string `json:"modelVersion"`
	IssuerConnector string `json:"issuerConnector"`
	SecurityToken   struct {
		Type        string `json:"@type"`
		TokenFormat string `json:"tokenFormat"`
		TokenValue  string `json:"tokenValue"`
	} `json:"securityToken"`
}

func fromConfig(idTokenJSONTemplate string) (*ServerHeader, error) {
	out := &ServerHeader{}
	data := []byte(idTokenJSONTemplate)
	if err := json.Unmarshal(data, &out); err != nil {
		return nil, malformedServerHeaderTemplateError(err)
	}
	return out, nil
}

func populateServerHeader(config *ServerHeader, idToken string,
	err error) error {
	if err != nil {
		return err
	}

	config.Issued = time.Now().Format(time.RFC3339)
	config.SecurityToken.TokenValue = idToken

	id, err := uuid.NewRandom()
	if err != nil {
		return uuidGenError(err)
	}
	config.ID = fmt.Sprintf(config.ID, id)

	return nil
}

func serializeServerHeader(h *ServerHeader, err error) (string, error) {
	if err != nil {
		return "", err
	}

	jsonRep, err := json.Marshal(h)
	if err != nil {
		return "", serverHeaderSerializationError(err)
	}

	b64rep := base64.StdEncoding.EncodeToString([]byte(jsonRep))
	return b64rep, nil
}

// BuildServerHeader assembles the response header value containing
// the connector's ID token we got from DAPS.
func BuildServerHeader(idTokenJSONTemplate string,
	idToken string) (string, error) {
	h, err := fromConfig(idTokenJSONTemplate)
	err = populateServerHeader(h, idToken, err)
	return serializeServerHeader(h, err)
}

// errors boilerplate

func serverHeaderSerializationError(cause error) error {
	return fmt.Errorf("malformed ID token header: %v", cause)
}

func malformedServerHeaderTemplateError(cause error) error {
	return fmt.Errorf("malformed ID token JSON template: %v", cause)
}

func uuidGenError(cause error) error {
	return fmt.Errorf("failed to generate v4 uuid: %v", cause)
}
