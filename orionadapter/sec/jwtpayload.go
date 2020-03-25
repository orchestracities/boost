package token

import (
	"math"
	"strconv"
	"time"

	"github.com/dgrijalva/jwt-go"
)

// JwtPayload holds JWT claims (token's payload block) in a map keyed by
// claim name.
type JwtPayload map[string]interface{}

func fromMapClaims(t *jwt.Token) JwtPayload {
	if t == nil {
		return JwtPayload{}
	}
	claims, ok := t.Claims.(jwt.MapClaims)
	if !ok {
		return JwtPayload{}
	}
	return (JwtPayload)(claims) // (*)
}

// (*) Like JwtPayload, MapClaims has an underlying type of
// map[string]interface{}, so the cast is actually safe.

// FromRaw extracts the payload of the specified JWT without doing any
// signature validation. If the input JWT is malformed, the returned
// payload will be empty.
func FromRaw(encodedToken string) JwtPayload {
	p := new(jwt.Parser)
	token, _, err := p.ParseUnverified(encodedToken, jwt.MapClaims{})
	if err != nil {
		return JwtPayload{}
	}
	return fromMapClaims(token)
}

// IsEmpty returns true just in case the payload contains no claims.
func (p JwtPayload) IsEmpty() bool {
	return len(p) == 0
}

// Scopes returns the 'scopes' array in the JWT payload. If there's no
// 'scopes' array or it isn't an array of strings, then return an empty
// slice.
func (p JwtPayload) Scopes() []string {
	switch scopes := p["scopes"].(type) {
	case []interface{}:
		return maybeStringSlice(scopes)
	default:
		return []string{}
	}
}

// ExpiresIn tells for how many seconds from now the token is still valid
// by looking at the 'exp' standard claim. If there's no 'exp' field, then
// return 0.
func (p JwtPayload) ExpiresIn() uint64 {
	now := toUint64(time.Now().Unix())
	exp := p.ExpirationTime()
	if exp <= now {
		return 0
	}
	return exp - now
}

// ExpirationTime reads the value of the 'exp' standard claim. If there's no
// 'exp' field, then return 0.
func (p JwtPayload) ExpirationTime() uint64 {
	return toUint64(p["exp"])
}

// conversion functions

func toUint64(x interface{}) uint64 {
	switch x.(type) {
	case int8:
		return intToUint64(int64(x.(int8)))
	case int16:
		return intToUint64(int64(x.(int16)))
	case int32:
		return intToUint64(int64(x.(int32)))
	case int64:
		return intToUint64(x.(int64))
	case int:
		return intToUint64(int64(x.(int)))
	case uint8: // NB in Go, byte = uint8
		return uint64(x.(uint8))
	case uint16:
		return uint64(x.(uint16))
	case uint32:
		return uint64(x.(uint32))
	case uint64:
		return x.(uint64)
	case uint:
		return uint64(x.(uint))
	case float32:
		return floatToUint64(float64(x.(float32)))
	case float64:
		return floatToUint64(x.(float64))
	case string:
		return stringToUint64(x.(string))
	default:
		return 0
	}
}

func intToUint64(x int64) uint64 {
	if x <= 0 {
		return 0
	}
	return uint64(x)
}

func floatToUint64(x float64) uint64 {
	y := math.Floor(x)
	if y <= 0 {
		return 0
	}
	if y >= math.MaxUint64 {
		return math.MaxUint64
	}
	return uint64(y)
}

func stringToUint64(x string) uint64 {
	y, err := strconv.ParseFloat(x, 64)
	if err != nil {
		return 0
	}
	return floatToUint64(y)
}

func maybeStringSlice(xs []interface{}) []string {
	size := len(xs)
	ys := make([]string, size, size)
	for i, x := range xs {
		if y, ok := x.(string); ok {
			ys[i] = y
		} else {
			return []string{}
		}
	}
	return ys
}
