package cache

import (
	"errors"
	"math"
	"testing"
	"time"
)

func newC(t *testing.T) *rcache {
	c, err := newRCache(1, nil)
	if err != nil {
		t.Errorf("failed to instantiate cache: %v", err)
	}
	return c
}

func assertKeep(t *testing.T, c *rcache, key, val string) {
	if ok := c.keep(key, val); !ok {
		t.Errorf("didn't keep (%v, %v)", key, val)
	}
}

func assertLookup(t *testing.T, c *rcache, key, expectedVal string) {
	got, found := c.lookup(key)
	if !found {
		t.Errorf("found = false, got: %v", got)
	}
	if got != expectedVal {
		t.Errorf("want: %v; got: %v", expectedVal, got)
	}
}

func TestStoreAndRetrieve(t *testing.T) {
	c := newC(t)

	assertKeep(t, c, "k1", "v1")
	assertKeep(t, c, "k2", "v2")

	// wait for values to pass through buffers
	time.Sleep(10 * time.Millisecond)

	assertLookup(t, c, "k1", "v1")
	assertLookup(t, c, "k2", "v2")
}

func TestReplace(t *testing.T) {
	c := newC(t)

	assertKeep(t, c, "k", "v1")
	assertKeep(t, c, "k", "v2")

	// wait for value to pass through buffers
	time.Sleep(10 * time.Millisecond)

	assertLookup(t, c, "k", "v2")
}

func TestExpiry(t *testing.T) {
	c := newC(t)

	var ttlSecs uint64 = 1
	if ok := c.put("k", "v", 1, ttlSecs); !ok {
		t.Error("shouldn't have put item")
	}

	// wait for value to pass through buffers
	time.Sleep(10 * time.Millisecond)

	assertLookup(t, c, "k", "v")

	// wait for TTL to expire
	time.Sleep(time.Duration(ttlSecs+1) * time.Second)

	got, found := c.lookup("k")
	if found {
		t.Errorf("found = true, got: %v", got)
	}
}

func TestDropTooExpensiveItem(t *testing.T) {
	c := newC(t)
	if ok := c.put("k", "v", math.MaxInt64, 10); !ok {
		t.Error("should have accepted item")
	}

	// wait for value to pass through buffers
	time.Sleep(10 * time.Millisecond)

	got, found := c.lookup("k")
	if found {
		t.Errorf("found = true, got: %v", got)
	}
}

func TestCanHandleTooLargeTTL(t *testing.T) {
	c := newC(t)
	c.put("k", "v", 1, math.MaxUint64)

	// wait for value to pass through buffers
	time.Sleep(10 * time.Millisecond)

	assertLookup(t, c, "k", "v")
}

func TestLookupNonExistingEntry(t *testing.T) {
	c := newC(t)
	got, found := c.lookup("k")
	if found {
		t.Errorf("found, value: %v", got)
	}
}

func TestNewRCacheDoNothingOnInputError(t *testing.T) {
	previousErr := errors.New("prev err")
	c, err := newRCache(1, previousErr)
	if err != previousErr {
		t.Errorf("instantiated cache: %v %v", err, c)
	}
}

func TestLookupWhenNilRCache(t *testing.T) {
	var c *rcache = nil
	value, found := c.lookup("")
	if found {
		t.Errorf("found: %v", value)
	}
}

func TestPutWhenNilRCache(t *testing.T) {
	var c *rcache = nil
	if ok := c.put("k", "v", 1, 10); ok {
		t.Error("put value?!")
	}
}

func TestKeepWhenNilRCache(t *testing.T) {
	var c *rcache = nil
	if ok := c.keep("k", "v"); ok {
		t.Error("kept value?!")
	}
}
