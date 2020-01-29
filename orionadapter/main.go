package main

import (
	"fmt"
	"log"
	"net/http"
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

// TODO: refactor this mess!!

func token(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, "whoopsie.dapsie!!")
}

func runHTTPServerLoop(port string) {
	http.HandleFunc("/", token)
	addr := fmt.Sprintf(":%s", port)

	log.Fatal(http.ListenAndServe(addr, nil))
}

func runHTTP() {
	port := os.Args[2] // TODO let it bomb out if no arg?!
	go runHTTPServerLoop(port)
}
