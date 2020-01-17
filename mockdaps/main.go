package main

import (
	"os"

	"github.com/orchestracities/boost/mockdaps/daps"
)

func main() {
	port := os.Args[1] // let it bomb out if no port arg
	daps.Serve(port)
}
