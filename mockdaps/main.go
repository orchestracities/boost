package main

import (
	"os"

	"github.com/orchestracities/boost/mockdaps/daps"
)

func main() {
	port := os.Args[1]             // let it bomb out if no port arg
	serverPvtKeyFile := os.Args[2] // ditto for the other args
	serverCertFile := os.Args[3]
	clientCertFile := os.Args[4]
	daps.Serve(port, serverPvtKeyFile, serverCertFile, clientCertFile)
}
