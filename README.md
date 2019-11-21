BOOST4.0 EIDS FIWARE CIM Connector
==================================
> Messin' about w/ Istio to bridge EIDS and Fiware.


### The basic idea

An Istio custom adapter to intercept and validate IDS-DTH tokens. If the
token is valid the requests gets forwarded on to Orion, else it winds up
in the sink of no return :-)


### Dev env

* git
* go >= 1.13
* add $GOPATH/bin (usually ~/go/bin) to your $PATH
* protobuf compiler >= 3.10 (brew install protobuf)
* go protocol buffers plugins:
    - go get -u github.com/golang/protobuf/protoc-gen-go
    - go get -u github.com/pseudomuto/protoc-gen-doc/cmd/protoc-gen-doc
* gogo protocol buffers plugins:
    - go get github.com/gogo/protobuf/proto
    - go get github.com/gogo/protobuf/protoc-gen-gogoslick
    - go get github.com/gogo/protobuf/gogoproto
* minikube >= 1.5 (or docker k8s?)
* kubectl >= 1.7
* docker


### Taking it for a spin

For the brave:

    $ sh scripts/gen-config.sh
    $ go build ./...
    $ sh scripts/make-mix.sh
    $ sh scripts/populate-testdata.sh
    $ go run orionadapter/main.go 43210

Now our custom adapter is running and waiting for the Mixer server to
hook up. Bring up the Mixer server in a new terminal:

    $ sh scripts/run-mixer.sh

The script will only run on MacOS but it's trivial to tweak it to make
it work on other OSes too. Now you're ready to use the Mixer client to
send an IDS-DTH token to the Mixer server. Open a new terminal and run:

    $ sh scripts/send-token.sh my.fat.jwt

(This script too only works on MacOS but it's easy to port to other OSes.)

At this point you should be able to see a status OK being returned.
The adapter only checks the token has length >= 0, so the check should've
succeeded. Have a look at the other terminals to see what's going on and
also look at the adapter code. If you call the script with no args:

    $ sh scripts/send-token.sh

you should get a fat permission denied back.


### Current status

All the adapter scaffolding is in place, we just need to implement the
`validateToken` function by porting the Ballerina code over which shouldn't
be too hard. Coming soon:

* mutual TLS (almost there!)
* K8s + Istio + adapter local and cloud deployment (in progress)
* Istio gateway / virtual service to handle IDS / Fiware message translation
  (will take blood, sweat and tears :-)
