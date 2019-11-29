package handler

import (
	"errors"
	"strings"

	iauth "istio.io/istio/mixer/template/authorization"
	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/config"
)

func getConfig(r *iauth.HandleAuthorizationRequest) (*config.Params, error) {
	cfg := &config.Params{}

	if r.AdapterConfig == nil {
		noCfgMsg := "request contains no adapter config!"
		ilog.Errorf("%s", noCfgMsg)
		return nil, errors.New(noCfgMsg)
	}
	if err := cfg.Unmarshal(r.AdapterConfig.Value); err != nil {
		ilog.Errorf("error unmarshalling adapter config: %v", err)
		return nil, err
	}

	return cfg, nil
}

func getIdsDthHeader(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}

	headerName := strings.TrimSpace(p.IdsDthHeader)
	if len(headerName) == 0 {
		noCfgMsg := "no IDS-DTH header name in adapter config!"
		ilog.Errorf("%s", noCfgMsg)
		return "", errors.New(noCfgMsg)
	}

	return headerName, nil
}

// TODO: get rid of this after implementing proper token validation
func getIdsDthExpectedToken(p *config.Params, e error) (string, error) {
	if e != nil {
		return "", e
	}

	expected_token := strings.TrimSpace(p.IdsDthExpectedToken)
	if len(expected_token) == 0 {
		noCfgMsg := "no IDS-DTH token in adapter config!"
		ilog.Errorf("%s", noCfgMsg)
		return "", errors.New(noCfgMsg)
	}

	return expected_token, nil
}
