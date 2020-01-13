package token

import (
	"testing"
)

// RSA 256 keys and tokens generated on: https://jwt.io/

const pubKey = `-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSv
vkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHc
aT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIy
tvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0
e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWb
V6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9
MwIDAQAB
-----END PUBLIC KEY-----`

const privateKey = `-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWw
kWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mr
m/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEi
NQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV
3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2
QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQABAoIBACiARq2wkltjtcjs
kFvZ7w1JAORHbEufEO1Eu27zOIlqbgyAcAl7q+/1bip4Z/x1IVES84/yTaM8p0go
amMhvgry/mS8vNi1BN2SAZEnb/7xSxbflb70bX9RHLJqKnp5GZe2jexw+wyXlwaM
+bclUCrh9e1ltH7IvUrRrQnFJfh+is1fRon9Co9Li0GwoN0x0byrrngU8Ak3Y6D9
D8GjQA4Elm94ST3izJv8iCOLSDBmzsPsXfcCUZfmTfZ5DbUDMbMxRnSo3nQeoKGC
0Lj9FkWcfmLcpGlSXTO+Ww1L7EGq+PT3NtRae1FZPwjddQ1/4V905kyQFLamAA5Y
lSpE2wkCgYEAy1OPLQcZt4NQnQzPz2SBJqQN2P5u3vXl+zNVKP8w4eBv0vWuJJF+
hkGNnSxXQrTkvDOIUddSKOzHHgSg4nY6K02ecyT0PPm/UZvtRpWrnBjcEVtHEJNp
bU9pLD5iZ0J9sbzPU/LxPmuAP2Bs8JmTn6aFRspFrP7W0s1Nmk2jsm0CgYEAyH0X
+jpoqxj4efZfkUrg5GbSEhf+dZglf0tTOA5bVg8IYwtmNk/pniLG/zI7c+GlTc9B
BwfMr59EzBq/eFMI7+LgXaVUsM/sS4Ry+yeK6SJx/otIMWtDfqxsLD8CPMCRvecC
2Pip4uSgrl0MOebl9XKp57GoaUWRWRHqwV4Y6h8CgYAZhI4mh4qZtnhKjY4TKDjx
QYufXSdLAi9v3FxmvchDwOgn4L+PRVdMwDNms2bsL0m5uPn104EzM6w1vzz1zwKz
5pTpPI0OjgWN13Tq8+PKvm/4Ga2MjgOgPWQkslulO/oMcXbPwWC3hcRdr9tcQtn9
Imf9n2spL/6EDFId+Hp/7QKBgAqlWdiXsWckdE1Fn91/NGHsc8syKvjjk1onDcw0
NvVi5vcba9oGdElJX3e9mxqUKMrw7msJJv1MX8LWyMQC5L6YNYHDfbPF1q5L4i8j
8mRex97UVokJQRRA452V2vCO6S5ETgpnad36de3MUxHgCOX3qL382Qx9/THVmbma
3YfRAoGAUxL/Eu5yvMK8SAt/dJK6FedngcM3JEFNplmtLYVLWhkIlNRGDwkg3I5K
y18Ae9n7dHVueyslrb6weq7dTkYDi3iOYRW8HRkIQh06wEdbxt0shTzAJvvCQfrB
jg/3747WSsf/zBTcHihTRBdAv6OmdhV4/dD5YBfLAkLrd+mX7iE=
-----END RSA PRIVATE KEY-----`

// {alg: RS256}.{}.signature
const emptyToken = `eyJhbGciOiJSUzI1NiJ9.e30.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw`

var malformedPubKey = []struct {
	pubKey string
}{
	{""}, {"-----BEGIN PUBLIC KEY-----"},
	{"-----BEGIN PUBLIC KEY-----\n-----END PUBLIC KEY-----"},
	{"-----BEGIN PUBLIC KEY-----\n junk \n-----END PUBLIC KEY-----"},
}

func TestMalformedPubKey(t *testing.T) {
	for _, h := range malformedPubKey {
		if err := Validate(h.pubKey, emptyToken); err == nil {
			t.Errorf("should reject malformed key: %s", h.pubKey)
		}
	}
}

var malformedToken = []struct {
	jwt string
}{
	{""}, {"a"}, {"a.b"}, {"my.fat.jwt"},
	{"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9"},
	{"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9."},
	{"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0"},
	{"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0."},
}

func TestMalformedToken(t *testing.T) {
	for _, h := range malformedToken {
		if err := Validate(pubKey, h.jwt); err == nil {
			t.Errorf("should reject malformed token: %s", h.jwt)
		}
	}
}

var invalidSigningMethod = []struct {
	jwt string
}{
	// { alg: HS256 }.{ x: 1 }.signature
	{"eyJhbGciOiJIUzI1NiJ9.eyJ4IjoxfQ.N6Em6A_zTZAPeTH4UJr9Zkd5S3Z-RAcRVe49mvkkIeY"},
	// { alg: none }.{ x: 1 }.signature
	{"eyJhbGciOiJub25lIn0.eyJ4IjoxfQ.ueuMDyRksOPtPn3raLlo_eh6n5o9LIkN2Bgqe-t6kaA"},
}

func TestInvalidSigningMethod(t *testing.T) {
	for _, h := range invalidSigningMethod {
		if err := Validate(pubKey, h.jwt); err == nil {
			t.Errorf("should reject token with unsupported algo: %s", h.jwt)
		}
	}
}

var expiredToken = []struct {
	jwt string
}{
	// { alg: RS256 }.{ exp: 1 }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjF9.ao_xHdGIW2RZsWqDjnTo76bUkpNQAJ90a0TKQtxU58yktLnVPWwm0B56rfPWqETQJtcVzAXcZ_PgQgZLAQd9tVJhWwTocZj1GxfunsMIWzkP_ZG4EZEnIwfD7PHbLCskC4MJplo8XKOfaV5-mC9GTB-SYcHNqM_aVuxveCHevrQiAxmW4h4qxlTb1G-SwCl8CMGLtQ_HS8hG-VkkRynXpr5E2QleGwyQVlqh1XrS0RVi8pxTWdv75eCRS4SoICW9d8p-VLidyYO_sSSyi744EJXBxqw4sg8nMtXbXdtcIeqOYTa7v0eAuVgv1EaJjZYJbHzqOay0EjRC3qSp8zTxKQ"},
	// { alg: RS256 }.{ exp: 1578569936 }.signature (~= 09 Jan 2020 @ 12:30pm)
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE1Nzg1Njk5MzZ9.GrjfzcPdu6UwKbmXU6eeJhDeq4FnitkYFyXk1Yc6GrO42Lbo6lmD_p60CRxVswzzLCHWgNwUSV5YZEO8IVqDo6jDUC90EiyOo9eANC9Tn2qRSi_Xz6fqqjzD4jo_5iRZjZnhFoI4Uv45mdlYkDmT-TX2snjcmCXEgopK1OlwO7pofYYfSp2fVd1G72QwwN0BripcdXGlcuzzs4pM4jSmeLf8PeNLXAR5xpp1qLWrSeWHjqz_WJNsVlxLf24n2LlZfHGCFX3bstp2c_GJ8RKWmlUlTVU1Hd8E_3AfnA1Hx4x3CR2t7ZuakZ7UlbEXlSqUiNCcrTnQfb_wKDMj-WUfxw"},
}

func TestExpiredToken(t *testing.T) {
	for _, h := range expiredToken {
		if err := Validate(pubKey, h.jwt); err == nil {
			t.Errorf("should reject expired token: %s", h.jwt)
		}
	}
}

var validToken = []struct {
	jwt string
}{
	// { alg: RS256 }.{ exp: 0 }.signature
	// amazingly, this will pass validation since to jwt-go `exp = 0` is the
	// same as `exp` not being there! Oh deary deary...
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjB9.POjPdPLLautp3cfDm84d-Z6-PwdJ_PndU6OOezPpv45XP-lkvJPbMs8BeYD6g0VnepxFYujfJbU1_MsIDFdIZkR9Hay-gpqcBNvSYekuOWZYvUDqzgOgM4wPZ6TPK2apx8u1_dZrep_xT6LKY0D7Vc_8NZL-DTLndxxTXeg6hdw6nGO2K2I5SQTZzU2oXfbuljnM4n5ljOKKOLAn5AUbBWi_Sr86U6mW6XMeSGvjsjCIUopHYybJmnPtZcMIQyo66wmR-iECu86GwvoHVU2-qwRHmIG6YoiHGM6xAVbWcBi7Aix9H-1Wc2KrQ5eAqOyd_Ndp_agONXhKh8I78DbllA"},
	//{alg: RS256}.{}.signature
	{emptyToken},
	// { alg: RS256, typ: JWT }.{ exp: 33134745600, foo: bar }.signature
	//                                 ^ 01 Jan 3020 @ 00:00
	{"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjMzMTM0NzQ1NjAwLCJmb28iOiJiYXIifQ.caG4HuOFMgrHfVILUNsL1v9gIn4fLBr-F1H16ZPufzEbuwImVwu9LIH9Tt9osHpRBH9nUgfVikdyqNJH0KcGiY_oSjJkN_jCXdRakl_ZKK86Nd2abJkgYKPAQwcOhzIfsAV1Gk58SPyNPknTUILg-y-bv-n_86tYXuAdoobZaQRJHBVsFbH6TbVPy0caF6sZVxB43TJ04m-XJ_Lmnfg6QJQ3bI_e10mkzxe96qnQsEJVqBa-tn36EChFO19niYhwok8p6Y_kPhXcTgnDu8gvYRE4H6HqAWdwHxzNgRIHLTQRrEW9yDsyA-xyBFaYV-RtkOS-CBnfLHEoGzLIp_DnWw"},
}

func TestValidToken(t *testing.T) {
	for _, h := range validToken {
		if err := Validate(pubKey, h.jwt); err != nil {
			t.Errorf("should accept valid token: %s", h.jwt)
		}
	}
}
