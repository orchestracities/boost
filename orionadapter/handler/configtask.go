package handler

import (
	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
)

func extractConfig(r *od.HandleOrionadapterRequest) (
	params *config.Params, err *od.HandleOrionadapterResponse) {
	ilog.Infof("auth request: %v\n", r.Instance)

	params, cErr := GetConfig(r)
	if cErr != nil {
		ilog.Errorf("can't read config from request: %v", cErr)
		return nil, configError()
	}

	return params, nil
}

func configError() *od.HandleOrionadapterResponse {
	return adapterErrorResponse("invalid configuration")
}
