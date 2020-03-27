package jwt

import (
	"crypto/rsa"
	"fmt"
	"time"

	jot "github.com/dgrijalva/jwt-go"
)

// ToRsaPvtKey parses an RSA private key in PEM format.
func ToRsaPvtKey(pemRep string) (*rsa.PrivateKey, error) {
	keyBytes := []byte(pemRep)
	key, err := jot.ParseRSAPrivateKeyFromPEM(keyBytes)
	if err != nil {
		return nil, invalidPvtKeyError(err)
	}
	return key, nil
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

// MakeRS256SignedToken mints a JWT holding the specified standard claims
// signed with the given RSA 256 private key in PEM format.
func MakeRS256SignedToken(pvtKeyPemRep, subject, issuer, audience string,
	secondsBeforeExpiry uint32) (string, error) {
	key, err := ToRsaPvtKey(pvtKeyPemRep)
	if err != nil {
		return "", err
	}
	claims := standardClaims(subject, issuer, audience, secondsBeforeExpiry)
	token := jot.NewWithClaims(jot.SigningMethodRS256, claims)
	return token.SignedString(key)
}

func standardClaims(subject, issuer, audience string,
	secondsBeforeExpiry uint32) *jot.StandardClaims {
	now := time.Now().Unix()
	return &jot.StandardClaims{
		IssuedAt:  now,
		NotBefore: now,
		ExpiresAt: now + int64(secondsBeforeExpiry),
		Subject:   subject,
		Issuer:    issuer,
		Audience:  audience,
	}
}

// errors boilerplate

func invalidPvtKeyError(cause error) *jot.ValidationError {
	msg := fmt.Sprintf("invalid private key: %v", cause)
	return jot.NewValidationError(msg, jot.ValidationErrorUnverifiable)
}

func invalidPubKeyError(cause error) *jot.ValidationError {
	msg := fmt.Sprintf("invalid public key: %v", cause)
	return jot.NewValidationError(msg, jot.ValidationErrorUnverifiable)
}
