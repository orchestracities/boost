package cache

// NOTE. Cache eviction. We use separate caches for each type of thing
// we want to cache to avoid a situation where e.g. too many AuthZ entries
// push a DAPS ID token out of the cache.
type store struct {
	authz  *rcache
	daps   *rcache
	config *rcache
}

func newStore() (*store, error) {
	authz, err := newRCache(1e6, nil)
	daps, err := newRCache(1, err)
	config, err := newRCache(1, err)

	return &store{
		authz:  authz,
		daps:   daps,
		config: config,
	}, err
}

type noopCache struct{}

func (c *noopCache) lookup(key string) (value interface{}, found bool) {
	return nil, false
}

func (c *noopCache) put(key string, value interface{}, cost int64,
	ttlSeconds uint64) (ok bool) {
	return false
}

func (c *noopCache) keep(key string, value interface{}) (ok bool) {
	return false
}

func (s *store) AuthZ() cache {
	if s == nil {
		return &noopCache{}
	}
	return s.authz
}

func (s *store) Daps() cache {
	if s == nil {
		return &noopCache{}
	}
	return s.daps
}

func (s *store) Config() cache {
	if s == nil {
		return &noopCache{}
	}
	return s.config
}
