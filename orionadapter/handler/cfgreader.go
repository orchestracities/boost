package handler

import (
	"fmt"
	"strings"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
)

func missingConfigError() error {
	return fmt.Errorf("request contains no adapter config")
}

func configReadError(cause error) error {
	return fmt.Errorf("error reading adapter config: %v", cause)
}

func missingFieldError(fieldName string) error {
	return fmt.Errorf("no value for adapter config field: %s", fieldName)
}

// GetConfig extracts adapter configuration from the Mixer request.
func GetConfig(r *od.HandleOrionadapterRequest) (*config.Params, error) {
	cfg := &config.Params{}

	if r.AdapterConfig == nil {
		return nil, missingConfigError()
	}

	if err := cfg.Unmarshal(r.AdapterConfig.Value); err != nil {
		return nil, configReadError(err)
	}
	return cfg, nil
}

func ensureString(fieldName string, fieldValue string) (string, error) {
	v := strings.TrimSpace(fieldValue)
	if len(v) == 0 {
		return "", missingFieldError(fieldName)
	}
	return v, nil
}

func getIdsaPublicKey(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("IdsaPublicKey", p.IdsaPublicKey)
}

func getIdsaPrivateKey(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("IdsaPrivateKey", p.IdsaPrivateKey)
}

func getDaps(p *config.Params) *config.Daps {
	if p.Daps != nil {
		return p.Daps
	}
	return &config.Daps{}
}

func getDapsConnectorID(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("Daps.ConnectorId", getDaps(p).ConnectorId)
}

func getDapsConnectorAudience(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("Daps.ConnectorAudience", getDaps(p).ConnectorAudience)
}

func getDapsSecondsBeforeExpiry(p *config.Params, e error) (uint32, error) {
	if e != nil {
		return 0, e
	}
	return getDaps(p).SecondsBeforeExpiry, nil
}

func getDapsPrivateKey(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("Daps.PrivateKey", getDaps(p).PrivateKey)
}

func getDapsConnectorCertificate(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("Daps.ConnectorCertificate",
		getDaps(p).ConnectorCertificate)
}

func getDapsServerCertificate(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("Daps.ServerCertificate",
		getDaps(p).ServerCertificate)
}

func getDapsServerHost(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("Daps.ServerHost", getDaps(p).ServerHost)
}

func getIDTokenJSONTemplate(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("IdTokenJsonTemplate", p.IdTokenJsonTemplate)
}

func getAuthZ(p *config.Params) *config.AuthZ {
	if p.Authz != nil {
		return p.Authz
	}
	return &config.AuthZ{}
}

func isAuthZEnabled(p *config.Params) bool {
	return getAuthZ(p).Enabled
}

func getAuthZServerURL(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("AuthZ.ServerURL", getAuthZ(p).ServerUrl)
}

func getAuthZResourceID(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}
	return ensureString("AuthZ.ResourceID", getAuthZ(p).ResourceId)
}
