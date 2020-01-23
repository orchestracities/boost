package https

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io/ioutil"
)

// PemData is a string containing certificates or keys in PEM format.
type PemData string

// PemDataFromFile reads PEM data from the specified file.
func PemDataFromFile(path string) (PemData, error) {
	content, err := ioutil.ReadFile(path)
	if err != nil {
		return "", err
	}
	return PemData(content), nil
}

func (d *PemData) bytes() []byte {
	return []byte(*d)
}

// MutualTLSConfig holds the certificates and keys needed for mTLS.
type MutualTLSConfig struct {
	ClientCerts      []PemData
	ServerCert       PemData
	ServerCertPvtKey PemData
}

// FromFiles populates MutualTLSConfig with the PEM data in the given files.
func (c *MutualTLSConfig) FromFiles(
	serverPvtKeyFile, serverCertFile, clientCertFile string) error {
	svrKey, err1 := PemDataFromFile(serverPvtKeyFile)
	svrCrt, err2 := PemDataFromFile(serverCertFile)
	cltCrt, err3 := PemDataFromFile(clientCertFile)

	if err1 != nil || err2 != nil || err3 != nil {
		//msg :=
		return fmt.Errorf("can't read PEM file(s)\n:[%s]: %v\n[%s]: %v\n[%s]: %v",
			serverPvtKeyFile, err1,
			serverCertFile, err2,
			clientCertFile, err3)
	}

	c.ClientCerts = []PemData{cltCrt}
	c.ServerCert = svrCrt
	c.ServerCertPvtKey = svrKey

	return nil
}

func (c *MutualTLSConfig) buildCAPool() (*x509.CertPool, error) {
	pool := x509.NewCertPool()
	for i, cert := range c.ClientCerts {
		if !pool.AppendCertsFromPEM(cert.bytes()) {
			return nil, fmt.Errorf(
				"can't add client cert [%d] to server pool", i)
		}
	}
	return pool, nil
}

func (c *MutualTLSConfig) buildServerCert() (tls.Certificate, error) {
	return tls.X509KeyPair(c.ServerCert.bytes(), c.ServerCertPvtKey.bytes())
}

func newTLSConfig(
	clientCerts *x509.CertPool, svrCert tls.Certificate) *tls.Config {
	cfg := &tls.Config{
		ClientCAs:    clientCerts,
		ClientAuth:   tls.RequireAndVerifyClientCert,
		Certificates: []tls.Certificate{svrCert},
	}
	cfg.BuildNameToCertificate()

	return cfg
}

// ServerTLSConfig builds an HTTPS server configuration for mTLS.
func (c *MutualTLSConfig) ServerTLSConfig() (*tls.Config, error) {
	pool, err := c.buildCAPool()
	if err != nil {
		return nil, err
	}

	svrCert, err := c.buildServerCert()
	if err != nil {
		return nil, err
	}

	return newTLSConfig(pool, svrCert), nil
}
