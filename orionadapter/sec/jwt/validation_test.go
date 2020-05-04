package jwt

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
		if _, err := Validate(h.pubKey, emptyToken); err == nil {
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
		if _, err := Validate(pubKey, h.jwt); err == nil {
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
		if _, err := Validate(pubKey, h.jwt); err == nil {
			t.Errorf("should reject token with unsupported algo: %s", h.jwt)
		}
	}
}

var tokenWithNonNumericDateNbfExpFixtures = []struct {
	jwt string
}{
	// { alg: RS256 }.{ exp: "" }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOiIifQ.PODhwKYCyI5IHpSkdz5dbtt4kK8DSBjwyKv_DowTvi3xkcDiKih8PbdHUyoipZ_JSpx9k_sxG-glyd8_fM3ReFkMAuQ76HLq36R2EeUiJ0EFH7waiCstswpIDwZr14Bgy48fP0vavl8zMluc7ldICWsZRnsNYhzZiwihYn5SULmuIa5-eEos2eCBvwE1Gx7WZIpz8nlQyk41MRFTm5aNzLi7xYUS7imPnleDnihrcocMh2c7_KXd2wU8KTGKMpm6E7iSk-C23yMSGQlKCor3iM5t8fZ7VZPqGTtc6yuThOw7xykMUTPRPKkYRPSN2Xio5IYMIeZer-bWuLvbGoQAUA"},
	// { alg: RS256 }.{ exp: "xxx" }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOiJ4eHgifQ.nBNiHgr_TjawcZxDmQtt0f6kFju_TQxVRk0jsvilwZNp-Bi0HcMez30IK2jJ9YCEuCxUhU_iBpkmW5ruvYGLyqxQj1lvEj7s8mFNbEJayvbXPO1SD1YJY7fllz5WWmYNJb9TOOPc0L4vaRJY7kGIVbi03HEg8KNFUhGf8ozIiOhCUi71V_TSxpiAy9cE5hZGV1g3NF1_2qtP4ln0KNW8BFzxOsdLjvE4wks9IR7p73164_iuO5Yb4JvLZgNl5GMPwwiL4dgB21nF54R9NcKX0KEzEyO4JaDArtUjFVkvcSR6a71SeFxbhMnG6F38bZsAReE5unrvI8_W5ppR3t8qdA"},
	// { alg: RS256 }.{ exp: { value: 2578569936 } }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOnsidmFsdWUiOjI1Nzg1Njk5MzZ9fQ.WAF3OXve2De1s8inRo1Slqkek2CnEXm5YdcgISxiQNd6BD_mG7FKpRr-j6ilK7L8YspT58L4L0CK9V57PaWEV5aYSvUUEqBbOUrbOBk8N3_c7dijHS-ebNE3skPqw7cVOIun86Em8RENWbtns-L8ni5TBaaUO_Oy_jPvN-Mqa4gi6lLjTaMlJncjH7VcjYC0Lt5MrMu5WSzebIaYDmCZRltMyreIjoOT1E5hfQVmPzjIRJ3K0emsktvBa4UsNQ1S0tv8t23PfrhiGfCJE3olR8yeE4KBYX42RpOvYFwaIhJhSrVj5neJ0eJbW0BWHWOR0Ch34SVKQBoZqe-PKVz0EQ"},
	// { alg: RS256 }.{ exp: [2578569936] }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOlsyNTc4NTY5OTM2XX0.EeiDTzd3GUEXfkiSDpNjbZNj6LzpWf5hQ2uERN-OrGivghpKK1bLASkPLxrJYy5uICqHigUL7b5uXMK4RWJTvQldNYocfdJd8cP2BPZEaDU2AM-NK9TdoNM-JuaMSJm7Ck4svuNe5Iu_5vDgiXqFc0X0Vvjlevef5IsKbCDEO-L8ypn2iHEyKolb9mDLllSgbZ4rVO4lW3oAWQu88FxbyO_PyUDCKK0zHT8nZuOXDeFc1Qxh2P0vig4kmSI59yXfUfZEsQ84FiiQX7RK88WrF439gfvWn1THnuZ-OVGnEC6PCcLziBgL0ICIpEzxnTb1qP7vtE2We4ZUJqgzP2jOAQ"},
	// { alg: RS256 }.{ nbf: "", exp: 2578569936 }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJuYmYiOiIiLCJleHAiOjI1Nzg1Njk5MzZ9.OtcUDCUo5KnNewI-wbfxXBLzxhpvaB73jQDaVIrvPK8gJ7nAaePSC1lYWfdyUfKLzGJf0SPdUvM-JwFhobAwgFz-VCFsXIzX-LUGUHw8sy421txiul2u_HN-iJZRHC8dUOIHRtytt0qU30h1YlCMVCmsRIAvKsfnhp86EobVr3TKIpgiYgalQYmcaCU4mOf2IMZpWQpjYYHuAH8ZlI8GDQAMk3wtm5gCXXfJwe2qDBHy1Dw6s95jiK4h30Bvt9TWSI1TN8Yw2V3MMVYkeIOhRfc9mvgr8r22Hv5DEzKjdVwdQXyoIaTS1XUWTBHbNdbgOVtsk4k3kS7JJPrW8W2Mew"},
	// { alg: RS256 }.{ nbf: " 123000 ", exp: 2578569936 }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJuYmYiOiIgMTIzMDAwICIsImV4cCI6MjU3ODU2OTkzNn0.IOKoP4jF2m3_T6WqP5Gth97dYZ3J9SNA0gHahzRENDUZsdq5VbIQR5E2CSbuHlVoq4SgD7VrEHV7ZRn_vwUm_MbeFbYSSH5yhYHb1f7Zqw28sFBoa0KJ54xhRc1aOqySFUphHJFToRrdZlEdi3nJw9ShTUxribb6B28a9B3UWeUaOXuk5eZlNUNkXHocDEVfUZdqNdm1Adt0WGRAJNXxekg8agHQKBIM_kogYvaFvpPhxYd_fZ5AsXMh90IsMETYeN_Lv5f22icuVxDoEUn8h6px0pK12kfi8ePGLSx9KIdluXUFjRu3nwyHBh4uil2NF9FBsfOK-ytPlclNHXabbA"},
	// { alg: RS256 }.{ nbf: " 123000 " }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJuYmYiOiIgMTIzMDAwICJ9.b7cDDI_3HklH78Ma5LttTqQR15eS0gGB4CLm1KOuwHy1nqiMCjP0m0NdW0F21OVYUSFm-slNwXulMb0Oksy4Qfy27wXFsHdaq4oaY2qHble6oWER7PWtM1PaYN3pNUc8jX_nNESDWqngeSqrcu1pSAvbHgsP1GAv-d9bjkfwcoVXjvdGxgbycmlV19s6c_YwLUyqLT9MN3hZU-DqmxkBeaxqQWmAMOYvv_d9Ok6lsPDFKVaK35WAWWpMrEn6xv97Bo4lcBlquSA5jLWFWQB3mKrC0ueh8c7B9PlzpiGMQ_WbW4Lh7sLIldB8uB3sF6lR83inGlKRa8XKXygNokZIBA"},
}

func TestTokenWithNonNumericDateNbfExp(t *testing.T) {
	for k, d := range tokenWithNonNumericDateNbfExpFixtures {
		if _, err := Validate(pubKey, d.jwt); err == nil {
			t.Errorf("[%v] should reject token: %s", k, d.jwt)
		}
	}
}

var expiredToken = []struct {
	jwt string
}{
	// { alg: RS256 }.{ exp: 0 }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjB9.POjPdPLLautp3cfDm84d-Z6-PwdJ_PndU6OOezPpv45XP-lkvJPbMs8BeYD6g0VnepxFYujfJbU1_MsIDFdIZkR9Hay-gpqcBNvSYekuOWZYvUDqzgOgM4wPZ6TPK2apx8u1_dZrep_xT6LKY0D7Vc_8NZL-DTLndxxTXeg6hdw6nGO2K2I5SQTZzU2oXfbuljnM4n5ljOKKOLAn5AUbBWi_Sr86U6mW6XMeSGvjsjCIUopHYybJmnPtZcMIQyo66wmR-iECu86GwvoHVU2-qwRHmIG6YoiHGM6xAVbWcBi7Aix9H-1Wc2KrQ5eAqOyd_Ndp_agONXhKh8I78DbllA"},
	// { alg: RS256 }.{ exp: 1 }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjF9.ao_xHdGIW2RZsWqDjnTo76bUkpNQAJ90a0TKQtxU58yktLnVPWwm0B56rfPWqETQJtcVzAXcZ_PgQgZLAQd9tVJhWwTocZj1GxfunsMIWzkP_ZG4EZEnIwfD7PHbLCskC4MJplo8XKOfaV5-mC9GTB-SYcHNqM_aVuxveCHevrQiAxmW4h4qxlTb1G-SwCl8CMGLtQ_HS8hG-VkkRynXpr5E2QleGwyQVlqh1XrS0RVi8pxTWdv75eCRS4SoICW9d8p-VLidyYO_sSSyi744EJXBxqw4sg8nMtXbXdtcIeqOYTa7v0eAuVgv1EaJjZYJbHzqOay0EjRC3qSp8zTxKQ"},
	// { alg: RS256 }.{ exp: 1578569936 }.signature (~= 09 Jan 2020 @ 12:30pm)
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE1Nzg1Njk5MzZ9.GrjfzcPdu6UwKbmXU6eeJhDeq4FnitkYFyXk1Yc6GrO42Lbo6lmD_p60CRxVswzzLCHWgNwUSV5YZEO8IVqDo6jDUC90EiyOo9eANC9Tn2qRSi_Xz6fqqjzD4jo_5iRZjZnhFoI4Uv45mdlYkDmT-TX2snjcmCXEgopK1OlwO7pofYYfSp2fVd1G72QwwN0BripcdXGlcuzzs4pM4jSmeLf8PeNLXAR5xpp1qLWrSeWHjqz_WJNsVlxLf24n2LlZfHGCFX3bstp2c_GJ8RKWmlUlTVU1Hd8E_3AfnA1Hx4x3CR2t7ZuakZ7UlbEXlSqUiNCcrTnQfb_wKDMj-WUfxw"},
	// { alg: RS256 }.{ nbf: 123000, exp: 1578569936 }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJuYmYiOjEyMzAwMCwiZXhwIjoxNTc4NTY5OTM2fQ.cnweW3YfROaFSl2Wb_9vUpcexzwmvnuy5FqwThvwF5kkQONem6XWLKKDo90qpGP-ol0kAHJcaCgSSVzb4lWLelaxRB8Q85sK36w_snlfCc1jRya1B9EOvJWNI0lWjkG5gs9TH6HTgiwappZSaiqrnUkSiy4ynmOJwlKV6q696FDTzn2TIfnd2reE4Vj3vbvKlAn5Aqas5RBDKLe0kcH5ozx0peDmizZhR60VDJRzrSfEiky3WMtsl49APup1RAtVUTzeOO-CNjKW2dBa7L3Tfmrnt-ARlb9AC627YcIzfPBZ4smxFzC4FRPVdvAOhOultqC534X1vILsEqqr2ZQDLQ"},
}

func TestExpiredToken(t *testing.T) {
	for _, h := range expiredToken {
		if _, err := Validate(pubKey, h.jwt); err == nil {
			t.Errorf("should reject expired token: %s", h.jwt)
		}
	}
}

var outOfAllowedTimeRangeToken = []struct {
	jwt string
}{
	// { alg: RS256 }.{ nbf: 1578569936, exp: 1 }.signature
	//                       ^ ~= 09 Jan 2020 @ 12:30pm
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjEsIm5iZiI6MTU3ODU2OTkzNn0.W47o6vnMAQpP4SVu6WxGw6tSguBiHJXk4kAOOqNdkEZk7VJ-Etw2wiR1aSbX_IlcfHwbSAq6ql-_Odben2L6RI_qb3SanPvLRpU9EWlLflvnoS_VGX8D39a1VvHpEbzYap_P0z-kKCMDGDH3t58Ah9sQhueFcoh1d9GAQ1bQrS2B_Ke_l8p9-OEjhKX5pI0rKL3EqSz_FpLtkykb4j27u6svtsjVBf-qNe81hghH5SAHnw4BUdm6ROkZDi7gqZlZAErvsQH2zY4NlbpLajMPoRCJm3BkgfAudSRpzz6uYY2MpHcNZEZfu5iMgp-ZUSVPs8-IHeSU2hxH_PkPk-eFOg"},
	// { alg: RS256 }.{ nbf: 1578569936, exp: 1578669936 }.signature
	//                 ^~= [09 Jan 2020 @ 12:30pm, 10 Jan 2020 @ 16:25pm]
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjE1Nzg2Njk5MzYsIm5iZiI6MTU3ODU2OTkzNn0.e1hCHHsZ3fHRUquqL7k1yOUNu5F-Yioo1Tmx5p7-I2dEWB2mh8fpDE_iUnZUyGUMn8xcipOWfA1222RG-fP6kzHt3NvrcWWwdLoIsk4E7TGLxQ8_51WVmdy-EPY60UBl23nNKPpHMwzS5IBpeRjfzmhOSbFryx5PyOU5_lrnlNM4IV-5-pgxbtZY3ojAexnGhvLacQ82ZG6Bnw4uRH81z6NySts5-6FuWIQotK-Iae1VbWf6kRZVOS0_jII-JqpBWByoxOcsT3vIVnBsG-8PL-C6RfY85LTWYrx_DTYW0SV8g-1S-b5r_6MYuiLpB7bOuMX28qK7NNthZquI4K2BEg"},
	// { alg: RS256 }.{ nbf: 2578569936, exp: 2678669936 }.signature
	//                 ^~= [17 Sep 2051 @ 15:25pm, 19 Nov 2054 @ 03:58am]
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOjI2Nzg2Njk5MzYsIm5iZiI6MjU3ODU2OTkzNn0.GrvOKhpgl-zRNWPO_ljhgxsxNuoUw2tNFEjBMfdw8b3c9q1K64QBVogUGhali66otU1rIz3UQMBZRuakjrTxh3UfzlC8jAXa7ePxuU5-UbQkhh4NFRvdwAJ_VBOah_L8LEDBWw6jOqIa-sVZypIOxpXQt5X0fj3kDGTV8k6jQifO4Zl2lt8xEq5eeQ2tz3Vm6SbVGiphzDJG3dWrdx4AwWcVP9rSvypKxAQ3rzGttnpq9JyMlAb27BP6wL0jNax8KOkGGoryneoTemQvaoeQ4-sXcGCP5wnqxt6J8z2hFnT3IHtajDP_sgZFZiQ7VhEJnlPPzyMEYAyU8nlc7ubRHQ"},
}

func TestOutOfAllowedTimeRangeToken(t *testing.T) {
	for _, h := range outOfAllowedTimeRangeToken {
		if _, err := Validate(pubKey, h.jwt); err == nil {
			t.Errorf("should reject token not in [nbf, exp]: %s", h.jwt)
		}
	}
}

var validToken = []struct {
	jwt string
}{
	//{alg: RS256}.{}.signature
	{emptyToken},
	// { alg: RS256 }.{ exp: null }.signature
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOm51bGx9.IdVs8sXE-e3C6fujBpWDtL3UQ2mRcs7KHNTs451XpwJOaIBSg0xkKheuuoASbGhEU4ON3SnUEEU2CTpjWe4dO_XqzHP0FjIRtc-HnL1D-i6mEnjkAW4HWlpcxVSEOYPPBkmDpj4o386PNnOXqdJxwzv_Kp1vddbWYrtn1H78gy1Y-0VQtbxlde_EWMukBEx3jz9iqZHW0f50hb4ANJDPmWDcxI_3lvScB_O_4lPF6xjaIV2CdMA5T3RGjgWY2692U87JGsynlC5Tg-hYP-hg0AwcwVAIdKrNaGOS4QYYAGYo1SAVhvjl92TbY_wLaf6GrCr_CdelBh1BRhUHc8Pj3w"},
	// { alg: RS256, typ: JWT }.{ exp: 33134745600, foo: bar }.signature
	//                                 ^ 01 Jan 3020 @ 00:00
	{"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjMzMTM0NzQ1NjAwLCJmb28iOiJiYXIifQ.caG4HuOFMgrHfVILUNsL1v9gIn4fLBr-F1H16ZPufzEbuwImVwu9LIH9Tt9osHpRBH9nUgfVikdyqNJH0KcGiY_oSjJkN_jCXdRakl_ZKK86Nd2abJkgYKPAQwcOhzIfsAV1Gk58SPyNPknTUILg-y-bv-n_86tYXuAdoobZaQRJHBVsFbH6TbVPy0caF6sZVxB43TJ04m-XJ_Lmnfg6QJQ3bI_e10mkzxe96qnQsEJVqBa-tn36EChFO19niYhwok8p6Y_kPhXcTgnDu8gvYRE4H6HqAWdwHxzNgRIHLTQRrEW9yDsyA-xyBFaYV-RtkOS-CBnfLHEoGzLIp_DnWw"},
	// { alg: RS256 }.{ exp: "2578569936" }.signature
	//                        ^ 2578569936 ~= 17 Sep 2051 @ 15:25pm
	// we accept numeric date string
	{"eyJhbGciOiJSUzI1NiJ9.eyJleHAiOiIyNTc4NTY5OTM2In0.jthpSiEr7aJPe5f5SvX0fkz8LxVA8X_VXU8E72c3FOq_cqAQMnQpRZ-Y5PHhmzjdjTyXAiAGxYL2bzRh0zyJRr5WiVA4nfxDeWf-rvAB3iv-_sPuSRzLp5d47vl7haka6NhLQo82_Utjpehw4JFQyWGasmr4TXSvs52iWX375nnlJso4h8RBf7685njW3DY0WNiMmRyjmFfCuq6pV7xdNzW5pWn2xXu4E9kAmRW-rNS3USCwboxqDpUanQ2cuOzNhXNBNRh34okPXaISsh94vCyHPMzvMhuZ7FhTsfbp1lTA4mJ0mJ5fQP-V8ki1H7HK0k7oa9h0XNSIVByoHGK14Q"},
}

func TestValidToken(t *testing.T) {
	for k, h := range validToken {
		if _, err := Validate(pubKey, h.jwt); err != nil {
			t.Errorf("[%v] should accept valid token: %s", k, h.jwt)
		}
	}
}
