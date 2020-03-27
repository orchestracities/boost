package cache

// NOTE. Concurrency. Treat this var as a const, you can't touch this :-)
var cached *store

// Init gets the cache infrastructure ready for use.
// It must be the first call to this module and must be done in the main
// function before any other thread can possibly call the functions this
// module exports.
func Init() error {
	s, err := newStore()
	if err != nil {
		return err
	}

	cached = s
	return nil
}
