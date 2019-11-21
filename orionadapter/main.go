package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/orchestracities/boost/orionadapter/grpc"
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
