Caching
-------
> Improving latency.

The adapter aggressively caches DAPS identities and AuthZ authorisation
decisions as Mixer built-in attribute caching alone is not enough to
achieve acceptable latency levels.

The caching component exposes a typed, map-like interface which shields
the rest of the adapter code from the inner details and whether the
backing cache is in memory or distributed. Presently, the implementation
uses an in-memory backing store, [Ristretto][ristretto], which features
sophisticated caching policies, high-performance concurrent access and
correctness guarantees under concurrent usage. The adapter caches three
kinds of items: DAPS identity tokens, AuthZ decisions and adapter configuration.
Each category of items has its own backing store in order to cater
for different usage patterns, e.g. there are different cache eviction
policies to avoid a situation where too many AuthZ entries push a DAPS
token out of the cache. Below is a description of the features of each
cache.


### DAPS identity cache

After retrieving a provider identity token from DAPS, the adapter
keeps it until it expires therefore the adapter will not call again
the DAPS server until then. Expiry is that specified by the `exp`
field of the DAPS JWT. If `exp` is in the past or there is no `exp`
field, the token will not be cached. Note that DAPS returns a JSON
object with the actual JWT and an additional `expires_in` JSON field.
The `expires_in` field is never taken into consideration because these
data are not digitally signed. Consequently, in a situation where
`expires_in` contained a time in the future but the value of `exp`
were not in the future, the token would not be cached.


### AuthZ decision cache

AuthZ calls are identified by KeyRock user JWT, IDS consumer JWT and
other relevant call inputs (HTTP method/path, FiWare service, etc.)
so that any two calls are considered equivalent just in case they
have the same inputs. After learning about an AuthZ decision for a
set of inputs, the adapter caches the authorisation decision for `t`
seconds, where `t` is the least of: consumer JWT expiry (computed
from the `exp` field in the DAPS token), user JWT expiry (computed
from `exp` in the KeyRock token), and a configurable maximum value.
Subsequently, if another HTTP request is processed that requires an
equivalent AuthZ call, the adapter will use the cached decision unless
it has expired, in which case it will call AuthZ again. Cache growth
is capped at `1GB` to prevent memory exhaustion and admission/eviction
policies try to maximise cache hit ratio.


### Adapter configuration cache

The adapter caches its own configuration too. This feature is only
needed to support the stopgap solution (mentioned earlier, see [#25][gh-25])
to dynamically generated HTTP headers for outgoing service requests
(see [#24][gh-24]) and the implementation can be discarded as soon
as a better option becomes available. This cache is the simplest thus
far: it only stores one item at a time (the adapter configuration)
until a fresher configuration becomes available at which point the
old one is replaced with the new. This is done every time the Mixer
calls the adapter as the gRPC call payload contains the latest configuration.




[gh-24]: https://github.com/orchestracities/boost/issues/24
    "Dynamic HTTP header for outbound traffic"
[gh-25]: https://github.com/orchestracities/boost/pull/25
    "Stopgap solution to ID token for outbound requests"
[ristretto]: https://github.com/dgraph-io/ristretto
