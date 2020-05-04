package jwt

import (
	"testing"
)

// Used below key to generate tokens on: https://jwt.io/

const hmacSecret = `d3eafd0101866b21`

// {alg: HS256}.{}.signature
const emptyHmacToken = `eyJhbGciOiJIUzI1NiJ9.e30.E0k7SthTgqwa1QcqczqoDKAkuvsiNO07uRebhiAQ_iw`

var invalidHmacKey = []struct {
	secret string
}{
	{""}, {"wannabe-key"}, {"d" + hmacSecret},
}

func TestInvalidHmacKey(t *testing.T) {
	for _, h := range invalidHmacKey {
		if _, err := ValidateHMAC(h.secret, emptyHmacToken); err == nil {
			t.Errorf("should reject invalid key: %s", h.secret)
		}
	}
}

var malformedHmacToken = []struct {
	jwt string
}{
	{""}, {"a"}, {"a.b"}, {"my.fat.jwt"},
	{"eyJhbGciOiJIUzI1NiJ9"},
	{"eyJhbGciOiJIUzI1NiJ9."},
	{"eyJhbGciOiJIUzI1NiJ9.e30"},
	{"eyJhbGciOiJIUzI1NiJ9.e30."},
}

func TestMalformedHmacToken(t *testing.T) {
	for _, h := range malformedHmacToken {
		if _, err := ValidateHMAC(hmacSecret, h.jwt); err == nil {
			t.Errorf("should reject malformed token: %s", h.jwt)
		}
	}
}

var hmacInvalidSigningMethod = []struct {
	jwt string
}{
	// { alg: RS256 }.{ x: 1 }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJ4IjoxfQ.aa5ihSrjMedJp8oG7D3pXFQO2cyAC1Y6yAWsvZaKbphGv_bb19XqTr26acSv4sn7p8JZhMYOnW9PiQ-EtkqSbT38wHjCP7fvu9Xg2QOy9kVN8qN4tt_IG9qLtaahmGzvI0DxhY02LJrRBrttBEzcAvYpXqcuuuHnzvg6XMQ81v_zLiGIR0-xmCeFLAZydCXNiLaSP6Cafhfi6Ki8KmijY0HGDyAX__JzGtUkXu7nuhUzAYBSHyajC_IYmmoRM15TfquN9u6vjNnce43W9wNCeEW5wep34dHt5GvUQ_gfwI7HsBaePrRromBx858GUriJnwzBo6O4w_pnJIiPVrAfbg"},
	// { alg: none }.{ x: 1 }.signature
	{"eyJhbGciOiJub25lIn0.eyJ4IjoxfQ.ueuMDyRksOPtPn3raLlo_eh6n5o9LIkN2Bgqe-t6kaA"},
}

func TestHmacInvalidSigningMethod(t *testing.T) {
	for _, h := range hmacInvalidSigningMethod {
		if _, err := ValidateHMAC(hmacSecret, h.jwt); err == nil {
			t.Errorf("should reject token with unsupported algo: %s", h.jwt)
		}
	}
}

var hmacInvalidSignature = []struct {
	jwt string
}{
	// { alg: HS256 }.{ x: 1 }.signature
	// secret: k
	{"eyJhbGciOiJIUzI1NiJ9.eyJ4IjoxfQ.iq8XoEfJtCUsa1Cp_hIruHQl9ZRKHEg3fkp79GaZtv0"},
	// { alg: HS256 }.{ x: 1 }.signature
	// secret: wannabe-secret
	{"eyJhbGciOiJIUzI1NiJ9.eyJ4IjoxfQ.Y08KCuqC1Dkf40l45IuLu0jXXDf31NWr_Crz7OijOIQ"},
}

func TestHmacInvalidSignature(t *testing.T) {
	for _, h := range hmacInvalidSignature {
		if _, err := ValidateHMAC(hmacSecret, h.jwt); err == nil {
			t.Errorf("should reject token with invalid signature: %s", h.jwt)
		}
	}
}

var hmacExpiredToken = []struct {
	jwt string
}{
	// { alg: HS256 }.{ exp: 0 }.signature
	{"eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjB9.w4syZjmIvYTTez_arMtAZshaWVoOWGlww3VAJZvTXkM"},
	// { alg: HS256 }.{ exp: 1 }.signature
	{"eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjF9.s2gjeoKI5LUgqXbo2t8JPCJZD0AEEGWJNdNh-I80mac"},
	// { alg: HS256 }.{ exp: 1578569936 }.signature (~= 09 Jan 2020 @ 12:30pm)
	{"eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1Nzg1Njk5MzZ9.uxa3ApVAf-Ef6Vhoyd2hwdEuo-PSVP-1yvvomLplVkg"},
}

func TestHmacExpiredToken(t *testing.T) {
	for _, h := range hmacExpiredToken {
		if _, err := ValidateHMAC(hmacSecret, h.jwt); err == nil {
			t.Errorf("should reject expired token: %s", h.jwt)
		}
	}
}

var validHmacToken = []struct {
	jwt string
}{
	//{alg: HS256}.{}.signature
	{emptyHmacToken},
	// { alg: HS256, typ: JWT }.{ exp: 33134745600, foo: bar }.signature
	//                                 ^ 01 Jan 3020 @ 00:00
	{"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjMzMTM0NzQ1NjAwLCJmb28iOiJiYXIifQ._lIWRaQJLRLenzfKMQlMLDr-y_ro7v2SxRqJwKQ0-kU"},
}

func TestValidHmacToken(t *testing.T) {
	for _, h := range validHmacToken {
		if _, err := ValidateHMAC(hmacSecret, h.jwt); err != nil {
			t.Errorf("should accept valid token: %s", h.jwt)
		}
	}
}
