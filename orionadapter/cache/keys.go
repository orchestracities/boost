package cache

import (
	"errors"
	"fmt"

	"github.com/orchestracities/boost/orionadapter/sec/authz"
	"github.com/orchestracities/boost/orionadapter/sec/consumer"
)

const adapterConfigKey = "adapterConfigKey"
const dapsIDTokenKey = "dapsIDTokenKey"

func authZCallKey(idsConsumerHeader string, callParams *authz.Request) (
	key string, consumerJWT string, err error) {
	consumerJWT, err = consumer.ReadToken(idsConsumerHeader)
	if err != nil {
		return "", "", err
	}
	if callParams == nil {
		return "", "", errors.New("authZCallKey: nil callParams")
	}

	return makeKey(consumerJWT, callParams), consumerJWT, nil
}

// NOTE. Fast hashing. Ristretto's default KeyToHash function converts a
// string to a []byte and then uses xxHash to get the actual hash which
// is great. So we only need to convert our structs to string.
func makeKey(consumerJWT string, callParams *authz.Request) string {
	return fmt.Sprintf("(%v, %v)", consumerJWT, callParams)
}
