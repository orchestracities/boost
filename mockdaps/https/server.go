package https

import (
	"crypto/tls"
	"net"
	"net/http"
)

func runServerLoop(svr *http.Server, cfg *tls.Config, feedback chan error) {
	tcpSocket, err := net.Listen("tcp", svr.Addr)
	if err != nil {
		feedback <- err
		return
	}
	tlsSocket := tls.NewListener(tcpSocket, cfg)
	if err != nil {
		feedback <- err
		return
	}

	defer tcpSocket.Close()
	feedback <- svr.Serve(tlsSocket)
}

// Run starts an HTTPS server in a separate goroutine, returning a channel
// where you can wait for server exit.
func Run(server *http.Server, cfg *tls.Config) chan error {
	feedback := make(chan error)
	go runServerLoop(server, cfg, feedback)
	return feedback
}
