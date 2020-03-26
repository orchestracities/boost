package cache

import (
	"math"
	"time"

	"github.com/dgraph-io/ristretto"
)

type cache interface {
	// lookup value for key if any.
	lookup(key string) (value interface{}, found bool)
	// put entry with cost and keep for ttl secs.
	put(key string, value interface{}, cost int64, ttlSeconds uint64) (ok bool)
	// keep entry until next put or eviction.
	keep(key string, value interface{}) (ok bool)
}

type rcache struct {
	backend *ristretto.Cache
}

func newRCache(expectedEntries uint32, err error) (*rcache, error) {
	if err != nil {
		return nil, err
	}
	backend, err := ristretto.NewCache(&ristretto.Config{
		// Number of keys to track frequency of.
		NumCounters: int64(10 * expectedEntries),
		// Maximum cost of cache (1GB).
		MaxCost: 1 << 30,
		// Number of keys per Get buffer.
		BufferItems: 64,
	})
	if err != nil {
		return nil, err
	}
	return &rcache{backend}, nil
}

// NOTE. Cache tuning. While the above may be reasonable defaults, there's
// no substitute for observing real prod workloads and then tune the cache
// accordingly. We should provide config knobs to play with at some point.

func (c *rcache) lookup(key string) (value interface{}, found bool) {
	if c == nil {
		return nil, false
	}
	return c.backend.Get(key)
}

func (c *rcache) put(key string, value interface{}, cost int64,
	ttlSeconds uint64) (ok bool) {
	if c == nil {
		return false
	}

	if ttlSeconds > math.MaxInt32 {
		ttlSeconds = math.MaxInt32 // time.Duration = int64 nanosec!
	}
	ttl := time.Duration(ttlSeconds) * time.Second

	c.backend.Del(key)
	return c.backend.SetWithTTL(key, value, cost, ttl)
}

func (c *rcache) keep(key string, value interface{}) (ok bool) {
	return c.put(key, value, 1, 0)
	// NOTE.
	// * Cost. A cost of 0 means: use default cost function. So we set it to 1.
	// * TTL. A TTL of 0 means: value never expires.
}
