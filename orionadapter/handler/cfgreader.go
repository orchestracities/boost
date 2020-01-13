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

func getConfig(r *od.HandleOrionadapterRequest) (*config.Params, error) {
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
