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

func outsideAllowedTimeIntervalError() *jot.ValidationError {
	msg := "current time not in [nbf, exp) interval"
	return jot.NewValidationError(msg, jot.ValidationErrorClaimsInvalid)
}

func ensureHmacSigning(key []byte) jot.Keyfunc {
	return func(t *jot.Token) (interface{}, error) {
		if _, ok := t.Method.(*jot.SigningMethodHMAC); !ok {
			return nil, unexpectedSigningMethodError(t.Header["alg"])
		}
		return key, nil
	}
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

func parseAndValidate(kf jot.Keyfunc, jwtData string) (Payload, error) {
	p := &jot.Parser{SkipClaimsValidation: true}
	// rolling out our own claims validation since jwt-go can't cut it.
	// see: https://github.com/orchestracities/boost/issues/14

	token, err := p.Parse(jwtData, kf)
	if err != nil {
		return nil, err
	}

	payload := fromMapClaims(token)
	if !payload.IsWithinAllowedTimeInterval() {
		return nil, outsideAllowedTimeIntervalError()
	}
	return payload, nil
}

// Validate the input JWT data and verify its provenance using the specified
// RSA public key in PEM format.
// Make sure the following is true:
//
// * token is well-formed;
// * token got signed with the private key paired to the input pub key;
// * current time falls within the 'nbf' ("not before" claim) and 'exp'
//   ("expiry time") bounds---see Payload.IsWithinAllowedTimeInterval
//   for details.
func Validate(pubKeyPemRep string, jwtData string) (Payload, error) {
	key, err := ToRsaPubKey(pubKeyPemRep)
	if err != nil {
		return nil, err
	}
	return parseAndValidate(ensureRsaSigning(key), jwtData)
}

// ValidateHMAC is a variant of Validate which uses an HMAC secret key for
// signature verification instead of an RSA public key. All the rest is the
// same.
func ValidateHMAC(secret string, jwtData string) (Payload, error) {
	key := []byte(secret)
	return parseAndValidate(ensureHmacSigning(key), jwtData)
}
