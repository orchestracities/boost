package cache

import (
	"errors"
	"fmt"

	token "github.com/orchestracities/boost/orionadapter/sec"
	"github.com/orchestracities/boost/orionadapter/sec/authz"
)

const adapterConfigKey = "adapterConfigKey"
const dapsIDTokenKey = "dapsIDTokenKey"

func authZCallKey(idsClientHeader string, callParams *authz.Request) (
	key string, clientJWT string, err error) {
	clientJWT, err = token.ReadClientToken(idsClientHeader)
	if err != nil {
		return "", "", err
	}
	if callParams == nil {
		return "", "", errors.New("authZCallKey: nil callParams")
	}

	return makeKey(clientJWT, callParams), clientJWT, nil
}

// NOTE. Fast hashing. Ristretto's default KeyToHash function converts a
// string to a []byte and then uses xxHash to get the actual hash which
// is great. So we only need to convert our structs to string.
func makeKey(clientJWT string, callParams *authz.Request) string {
	return fmt.Sprintf("(%v, %v)", clientJWT, callParams)
}
