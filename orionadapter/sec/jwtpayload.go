package token

import (
	"github.com/dgrijalva/jwt-go"
)

// JwtPayload holds JWT claims (token's payload block) in a map keyed by
// claim name.
type JwtPayload map[string]interface{}

func fromMapClaims(t *jwt.Token) JwtPayload {
	claims, ok := t.Claims.(jwt.MapClaims)
	if !ok {
		return JwtPayload{}
	}
	return (JwtPayload)(claims) // (*)
}

// (*) Like JwtPayload, MapClaims has an underlying type of
// map[string]interface{}, so the cast is actually safe.

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
