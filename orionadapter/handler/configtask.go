package handler

import (
	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/cache"
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

	if ok := cache.PutAdapterConfig(params); ok {
		ilog.Info("cached latest adapter config\n")
	} else {
		ilog.Error("failed to cache latest adapter config\n")
	}
	// TODO: get rid of above caching once this gets sorted:
	// - https://github.com/orchestracities/boost/issues/24

	return params, nil
}

func configError() *od.HandleOrionadapterResponse {
	return adapterErrorResponse("invalid configuration")
}
