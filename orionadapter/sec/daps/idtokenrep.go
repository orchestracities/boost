package daps

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"time"

	"github.com/google/uuid"
)

// ProviderHeader holds the IDS server header we generate.
type ProviderHeader struct {
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

func fromConfig(idTokenJSONTemplate string) (*ProviderHeader, error) {
	out := &ProviderHeader{}
	data := []byte(idTokenJSONTemplate)
	if err := json.Unmarshal(data, &out); err != nil {
		return nil, malformedProviderHeaderTemplateError(err)
	}
	return out, nil
}

func populateProviderHeader(config *ProviderHeader, idToken string,
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

func serializeProviderHeader(h *ProviderHeader, err error) (string, error) {
	if err != nil {
		return "", err
	}

	jsonRep, err := json.Marshal(h)
	if err != nil {
		return "", providerHeaderSerializationError(err)
	}

	b64rep := base64.StdEncoding.EncodeToString([]byte(jsonRep))
	return b64rep, nil
}

// BuildProviderHeader assembles the response header value containing
// the connector's ID token we got from DAPS.
func BuildProviderHeader(idTokenJSONTemplate string,
	idToken string) (string, error) {
	h, err := fromConfig(idTokenJSONTemplate)
	err = populateProviderHeader(h, idToken, err)
	return serializeProviderHeader(h, err)
}

// errors boilerplate

func providerHeaderSerializationError(cause error) error {
	return fmt.Errorf("malformed ID token header: %v", cause)
}

func malformedProviderHeaderTemplateError(cause error) error {
	return fmt.Errorf("malformed ID token JSON template: %v", cause)
}

func uuidGenError(cause error) error {
	return fmt.Errorf("failed to generate v4 uuid: %v", cause)
}
