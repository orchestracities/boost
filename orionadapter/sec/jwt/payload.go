package jwt

import (
	"time"

	jot "github.com/dgrijalva/jwt-go"
)

// Payload holds JWT claims (token's payload block) in a map keyed by
// claim name.
type Payload map[string]interface{}

func fromMapClaims(t *jot.Token) Payload {
	if t == nil {
		return Payload{}
	}
	claims, ok := t.Claims.(jot.MapClaims)
	if !ok {
		return Payload{}
	}
	return (Payload)(claims) // (*)
}

// (*) Like JwtPayload, MapClaims has an underlying type of
// map[string]interface{}, so the cast is actually safe.

// FromRaw extracts the payload of the specified JWT without doing any
// signature validation. If the input JWT is malformed, the returned
// payload will be empty.
func FromRaw(encodedToken string) Payload {
	p := new(jot.Parser)
	token, _, err := p.ParseUnverified(encodedToken, jot.MapClaims{})
	if err != nil {
		return Payload{}
	}
	return fromMapClaims(token)
}

// IsEmpty returns true just in case the payload contains no claims.
func (p Payload) IsEmpty() bool {
	return len(p) == 0
}

// ExpiresIn tells for how many seconds from now the token is still valid
// by looking at the 'exp' standard claim. If there's no 'exp' field, then
// return 0.
func (p Payload) ExpiresIn() uint64 {
	now := toUint64(time.Now().Unix())
	exp := p.ExpirationTime()
	if exp <= now {
		return 0
	}
	return exp - now
}

// ExpirationTime reads the value of the 'exp' standard claim. If there's no
// 'exp' field, then return 0.
func (p Payload) ExpirationTime() uint64 {
	return toUint64(p["exp"])
}

// Scopes returns the 'scopes' array in the JWT payload of a DAPS token.
// If there's no 'scopes' array or none of its elements is a string, then
// return an empty slice. Otherwise, return a slice with the string elements
// found in the 'scopes' array, in the same order in which they appear.
func (p Payload) Scopes() []string {
	return toListOfString(p["scopes"])
}

// Roles returns all KeyRock role names in sight from the JWT payload of a
// KeyRock token, removing any duplicates and empty strings.
// More accurately, consider the set of all name attributes of role objects
// found in top level 'organizations' and 'roles' array. Out of this set,
// list those names having a non-empty string value, sorting them in
// ascending alphabetical order.
func (p Payload) Roles() []string {
	roles := make([]map[string]interface{}, 0, 256)
	for _, org := range toListOfMap(p["organizations"]) {
		xs := toListOfMap(org["roles"])
		roles = append(roles, xs...)
	}
	ys := toListOfMap(p["roles"])
	roles = append(roles, ys...)

	names := collectStrings("name", roles)
	nameSet := make([]string, 0, len(names))
	for _, n := range dedupeStrings(names) {
		if n != "" {
			nameSet = append(nameSet, n)
		}
	}
	return nameSet
}
