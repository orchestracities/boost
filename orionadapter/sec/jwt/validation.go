package jwt

import (
	"crypto/rsa"
	"fmt"

	jot "github.com/dgrijalva/jwt-go"
)

func unexpectedSigningMethodError(algo interface{}) *jot.ValidationError {
	msg := fmt.Sprintf("unexpected signing method: %v", algo)
	return jot.NewValidationError(msg, jot.ValidationErrorSignatureInvalid)
}

func invalidPubKeyError(cause error) *jot.ValidationError {
	msg := fmt.Sprintf("invalid public key: %v", cause)
	return jot.NewValidationError(msg, jot.ValidationErrorUnverifiable)
}

// ToRsaPubKey parses an RSA public key in PEM format.
func ToRsaPubKey(pemRep string) (*rsa.PublicKey, error) {
	keyBytes := []byte(pemRep)
	key, err := jot.ParseRSAPublicKeyFromPEM(keyBytes)
	if err != nil {
		return nil, invalidPubKeyError(err)
	}
	return key, nil
}

func ensureRsaSigning(key *rsa.PublicKey) jot.Keyfunc {
	return func(t *jot.Token) (interface{}, error) {
		if _, ok := t.Method.(*jot.SigningMethodRSA); !ok {
			return nil, unexpectedSigningMethodError(t.Header["alg"])
		}
		return key, nil
	}
}

/* NOTE. Security.
Checking the signature algo ∂ (HMAC, RSA, ...) declared in the JWT `alg`
header is **critical**, since jwt-go will check the token according to ∂.
Specifically, we could say that in (very!) abstract terms, jwt-go implements
a signature verification function F: Algo x JWT -> Bool so that for a
JWT token t

    t's signature is valid <=> F(∂, t) = T   where ∂ = t.alg

Because of this, an attacker could use our public key K as an HS256 shared
secret to sign a forged token u:

	header = { alg: HS256, ... }
	payload = { ... }
	signature = hs256-sign(K, header + payload)

Since in this case F(HS256, u) = T, we'd be fooled into thinking u got
signed with the private key paired to K!!

For a better explanation of the problem, see e.g.
- https://auth0.com/blog/critical-vulnerabilities-in-json-web-token-libraries/
*/

// Validate the input JWT data and verify its provenance using the specified
// RSA public key in PEM format.
// Make sure the following is true:
//
// * token is well-formed;
// * token got signed with the private key paired to the input pub key;
// * if present, exp ("expires at") contains a date in the future;
// * if present, iat ("issued at") contains a date in the past;
// * if present, nbf ("not before") contains a date in the past.
//
// Warning. To jwt-go a field with a 0 value is the same as the field not
// being there! So e.g. this token
//    { alg: RS256 }.{ exp: 0 }.valid-rs256-signature
// passes validation even though it expired at the beginning of the epoch!
// Oh well.
func Validate(pubKeyPemRep string, jwtData string) (Payload, error) {
	key, err := ToRsaPubKey(pubKeyPemRep)
	if err != nil {
		return nil, err
	}
	token, err := jot.Parse(jwtData, ensureRsaSigning(key))
	if err != nil {
		return nil, err
	}
	return fromMapClaims(token), nil
}
