package endpoint

import (
	"context"
	"fmt"
	"net"

	"google.golang.org/grpc"

	od "github.com/orchestracities/boost/orionadapter/codegen/oriondata"
	"github.com/orchestracities/boost/orionadapter/handler"
)

type (
	// Server is a basic server interface as often used in GRPC.
	// TODO: is this I/F also expected by the ISTIO adapter?
	Server interface {
		// Addr returns the address the GRPC server is listening to.
		Addr() string
		// Close gracefully shuts down the server; used for testing.
		Close() error
		// Run starts the server loop and exits on receving a shutdown singnal,
		// i.e. any data sent over the shutdown channel.
		Run(shutdown chan error)
	}

	// OrionAdapter implements Server and dispatches the auth template hanlder.
	OrionAdapter struct {
		listener net.Listener
		server   *grpc.Server
	}
)

// ask compiler to check types are compatible.
var _ od.HandleOrionadapterServiceServer = &OrionAdapter{}

// HandleOrionadapter is the template hook we use to dispatch the
// call to our handler.
func (s *OrionAdapter) HandleOrionadapter(ctx context.Context,
	r *od.HandleOrionadapterRequest) (*od.HandleOrionadapterResponse, error) {
	return handler.Authorize(r)
}

// Addr returns the address the GRPC server is listening to.
func (s *OrionAdapter) Addr() string {
	return s.listener.Addr().String()
}

// Run starts the server loop and, on exit, sends any server error over the
// shutdown channel.
func (s *OrionAdapter) Run(shutdown chan error) {
	shutdown <- s.server.Serve(s.listener)
}

// Close gracefully shuts down the server; used for testing.
func (s *OrionAdapter) Close() error {
	if s.server != nil {
		s.server.GracefulStop()
	}

	if s.listener != nil {
		_ = s.listener.Close()
	}

	return nil
}

// NewOrionAdapter creates a new IBP adapter that listens at the given port.
func NewOrionAdapter(port int) (Server, error) {
	listener, err := net.Listen("tcp", fmt.Sprintf(":%v", port))
	if err != nil {
		return nil, fmt.Errorf("unable to listen on socket: %v", err)
	}
	s := &OrionAdapter{
		listener: listener,
	}
	fmt.Printf("listening on: %v\n", s.Addr())
	s.server = grpc.NewServer()
	od.RegisterHandleOrionadapterServiceServer(s.server, s)
	return s, nil
}
