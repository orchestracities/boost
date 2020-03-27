package main

import (
	"fmt"
	"os"
	"strconv"

	grpc "github.com/orchestracities/boost/orionadapter/endpoint"
)

func main() {
	port := readPortArg()
	s, err := grpc.NewOrionAdapter(port)
	if err != nil {
		fmt.Printf("unable to start server: %v", err)
		os.Exit(-1)
	}

	shutdown := make(chan error, 1)
	go func() {
		s.Run(shutdown)
	}()

	runHTTP()

	_ = <-shutdown
}

func readPortArg() int {
	if len(os.Args) > 1 {
		if port, err := strconv.Atoi(os.Args[1]); err == nil {
			return port
		}
	}
	return 0
}

// TODO: get rid of below code once this gets sorted:
// - https://github.com/orchestracities/boost/issues/24

func runHTTP() {
	port := os.Args[2] // TODO let it bomb out if no arg?!
	go grpc.RunHTTPServerLoop(port)
}
