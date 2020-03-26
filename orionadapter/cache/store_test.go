package cache

import (
	"testing"
	"time"
)

func assertNoop(t *testing.T, c cache) {
	if v, found := c.lookup("k"); found {
		t.Errorf("should've refused to lookup but got: %v", v)
	}
	if ok := c.keep("k", "v"); ok {
		t.Error("should've refused to keep")
	}
	if ok := c.put("k", "v", 1, 1); ok {
		t.Error("should've refused to put")
	}
}

func newS(t *testing.T) *store {
	s, err := newStore()
	if err != nil {
		t.Errorf("failed to instantiate store: %v", err)
	}
	return s
}

func TestNoopAuthZWhenNilStore(t *testing.T) {
	var s *store = nil
	assertNoop(t, s.AuthZ())
}

func TestAuthZStoreAndRetrieve(t *testing.T) {
	s := newS(t)
	assertKeep(t, s.AuthZ(), "k", "v")

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	assertLookup(t, s.AuthZ(), "k", "v")
}

func TestNoopDapsWhenNilStore(t *testing.T) {
	var s *store = nil
	assertNoop(t, s.Daps())
}

func TestDapsStoreAndRetrieve(t *testing.T) {
	s := newS(t)
	assertKeep(t, s.Daps(), "k", "v")

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	assertLookup(t, s.Daps(), "k", "v")
}

func TestNoopConfigWhenNilStore(t *testing.T) {
	var s *store = nil
	assertNoop(t, s.Config())
}

func TestConfigStoreAndRetrieve(t *testing.T) {
	s := newS(t)
	assertKeep(t, s.Config(), "k", "v")

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	assertLookup(t, s.Config(), "k", "v")
}
