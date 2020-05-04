package jwt

import (
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

// Standard claims.

// nbf returns the raw 'nbf' field value or nil if there's no 'nbf'.
func (p Payload) nbf() interface{} {
	return p["nbf"]
}

// exp returns the raw 'exp' field value or nil if there's no 'exp'.
func (p Payload) exp() interface{} {
	return p["exp"]
}

// ExpiresIn tells for how many seconds from now the token is still valid
// by looking at the 'exp' standard claim. If there's no 'exp' field or
// the value isn't a numeric date, then return 0.
// Any JSON number value counts as a numeric date (seconds since the epoch)
// as well as any string representation of a number---e.g. "2143" gets
// interpreted as 2143 seconds since the epoch but "21 43" does not.
func (p Payload) ExpiresIn() uint64 {
	now := secondsSinceEpoch()
	exp := p.ExpirationTime()
	if exp <= now {
		return 0
	}
	return exp - now
}

// ExpirationTime reads the value of the 'exp' standard claim. If there's no
// 'exp' field or the value isn't a numeric date, then return 0.
// Any JSON number value counts as a numeric date (seconds since the epoch)
// as well as any string representation of a number---e.g. "2143" gets
// interpreted as 2143 seconds since the epoch but "21 43" does not.
func (p Payload) ExpirationTime() uint64 {
	return toUint64(p.exp())
}

// IsWithinAllowedTimeInterval tells if the current time falls within the
// token's 'nbf' ("not before" claim) and 'exp' ("expiry time") bounds.
// More accurately, return true just in case both the below conditions hold
//
// 1. 'exp' isn't present or, if it is, specifies a date in the future.
// 2. 'nbf' isn't present or, if it is, doesn't specify a date in the
//    future.
//
// Notice that if either field is present but doesn't hold a numeric date
// value, then we return false since (1) and (2) don't hold true. Any JSON
// number value counts as a numeric date (seconds since the epoch) as well
// as any string representation of a number, e.g. "2143" gets interpreted
// as 2143 seconds since the epoch but "21 43" does not.
func (p Payload) IsWithinAllowedTimeInterval() bool {
	nbfCheck, expCheck := true, true
	now := secondsSinceEpoch()

	if p.nbf() != nil {
		nbf := toUint64(p.nbf()) // = 0 if not numeric or string-numeric
		nbfCheck = isNumeric(p.nbf()) && nbf <= now
	}
	if p.exp() != nil {
		expCheck = isNumeric(p.exp()) && now < p.ExpirationTime()
	}

	return nbfCheck && expCheck
}

// NOTE. IsWithinAllowedTimeInterval case analysis. (We've covered all the
// bases :-)
// Each of the nbf, exp field x can be in one of three states ∈ S = {0, 1, 2}
//
//     0 : not present; e.g. { x: null } or no x in {...}
//     1 : present but not numeric; e.g. { x: "wada wada" }, { x: [12323] }
//     2 : present and numeric; e.g. { x: 2234 }
//
// so there are nine possible states (n, e) ∈ S × S for the pair (nbf, exp).
// If a field is present than it must be numeric, so we return false for
// any of the states { (n, e) | n = 1  ∨  e = 1 } whereas the result will
// be true if there's nothing to check---(0, 0) state, when both fields
// aren't there or both have a nil value. We're left with another three
// states to deal with: (0, 2), (2, 0) and (2, 2). If nbf (exp) isn't
// present, then we base our decision on exp (nbf) only whereas if both
// nbf and exp are present and numeric we use both. The below decision table
// sums it all up.
//
//     nbf  exp
//     ---  ---
//      0    0   ==> T
//      0    1   ==> F
//      0    2   ==> now < exp
//      1    0   ==> F
//      1    1   ==> F
//      1    2   ==> F
//      2    0   ==> nbf ≤ now
//      2    1   ==> F
//      2    2   ==> nbf ≤ now < exp
//

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
