package handler

import (
	ilog "istio.io/pkg/log"

	"github.com/orchestracities/boost/orionadapter/codegen/config"
	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
	"github.com/orchestracities/boost/orionadapter/sec/daps"
)

func generateProviderHeader(params *config.Params) (
	header string, err *od.HandleOrionadapterResponse) {
	header, hErr := GenerateToken(params)
	if hErr != nil {
		ilog.Errorf("error generating provider header: %v\n", hErr)
		return "", tokenGenError()
	}

	return header, nil
}

// GenerateToken gets a new ID token from DAPS, puts it into the configured
// provider header JSON object and then Base64 encodes the JSON object.
func GenerateToken(p *config.Params) (string, error) {
	request, err := buildDapsIDRequest(p)
	idTokenTemplate, err := getIDTokenJSONTemplate(p, err)
	if err != nil {
		return "", err
	}

	idToken, err := request.IdentityToken()
	if err != nil {
		return "", err
	}

	return daps.BuildProviderHeader(idTokenTemplate, idToken)
}

func buildDapsIDRequest(p *config.Params) (*daps.IDRequest, error) {
	connectorID, err := getDapsConnectorID(p, nil)
	connectorAudience, err := getDapsConnectorAudience(p, err)
	secondsBeforeExpiry, err := getDapsSecondsBeforeExpiry(p, err)
	privateKey, err := getDapsPrivateKey(p, err)
	connectorCertificate, err := getDapsConnectorCertificate(p, err)
	serverCertificate, err := getDapsServerCertificate(p, err)
	serverHost, err := getDapsServerHost(p, err)

	if err != nil {
		return nil, err
	}

	r := &daps.IDRequest{
		ConnectorID:          connectorID,
		ConnectorAudience:    connectorAudience,
		SecondsBeforeExpiry:  secondsBeforeExpiry,
		PrivateKey:           privateKey,
		ConnectorCertificate: connectorCertificate,
		ServerCertificate:    serverCertificate,
		ServerHost:           serverHost,
	}
	return r, nil
}

func tokenGenError() *od.HandleOrionadapterResponse {
	return adapterErrorResponse("context broker token could not be generated")
}
