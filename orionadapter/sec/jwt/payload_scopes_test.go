package jwt

import (
	"reflect"
	"testing"
)

func TestMissingScopes(t *testing.T) {
	payload := Payload{}
	got := payload.Scopes()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestScopesWithEmptyArray(t *testing.T) {
	payload := Payload{
		"scopes": []interface{}{},
	}
	got := payload.Scopes()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestScopesWithIncompatibleType(t *testing.T) {
	payload := Payload{
		"scopes": []int{},
	}
	got := payload.Scopes()
	if got == nil {
		t.Errorf("want: empty slice; got: nil")
	}
	if size := len(got); size != 0 {
		t.Errorf("want: empty slice; got: %v", got)
	}
}

func TestScopesWithSomeValues(t *testing.T) {
	payload := Payload{
		"scopes": []interface{}{"a", "b"},
	}
	got := payload.Scopes()
	if got == nil {
		t.Errorf("want: two-entry slice; got: nil")
	}
	if size := len(got); size != 2 {
		t.Errorf("want: two-entry slice; got: %v", got)
	}
	if got[0] != "a" {
		t.Errorf("want: a; got: %v", got[0])
	}
	if got[1] != "b" {
		t.Errorf("want: b; got: %v", got[1])
	}
}

// jot is a JWT generated on jwt.io using keys in validation_test.go
func extractScopesFromJwt(t *testing.T, jot string) []string {
	payload, err := Validate(pubKey, jot)
	if err != nil {
		t.Errorf("unexpected JWT validation error: %v", err)
	}
	return payload.Scopes()
}

// tokens generated on jwt.io with keys in validation_test.go
var extractScopesFromJwtFixtures = []struct {
	token string
	want  []string
}{
	{
		// { alg: RS256 }.{ }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.e30.QHOtHczHK_bJrgqhXeZdE4xnCGh9zZhp67MHfRzHlUUe98eCup_uAEKh-2A8lCyg8sr1Q9dV2tSbB8vPecWPaB43BWKU00I7cf1jRo9Yy0nypQb3LhFMiXIMhX6ETOyOtMQu1dS694ecdPxMF1yw4rgqTtp_Sz-JfrasMLcxpBtT7USocnJHE_EkcQKVXeJ857JtkCKAzO4rkMli2sFnKckvoJMBoyrObZ_VFCVR5NGnOvSnLMqKrYaLxNHLDL_0Mxy_b8iKTiRAqyNce4tg8Evhqb3rPQcx9kMdwyv_1ggEVKQyiPWa3MkSBvBArgPghbJMcSJVMhtUO8M9BmNMyw`,
		want:  []string{},
	},
	{
		// { alg: RS256 }.{ scopes: {} }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.eyJzY29wZXMiOnt9fQ.UgdhJ4RJzogOmHWnuo6lKNsES6CSVDubprKSuf2eFOoXNEcVJ4JZF-OXxqGQOnyv-_W4evOnI0oUiSyYLIKIHRyWkOf_Q1pAKOS4SkCDBjOTdmLJ0F-5KN4jH9jnWvQl7tRGltgSF_Ckm03Pjy_sHKP68NkEPSkUj1ElqXV7WD0KrYF7wxKc1H7ALLH54lH6i-SxDi10DxmZ3mPhDg1ITu1HF3GqhQupAYEP1tN99HEhG6SyhTOtgRzOJzowAGv-5Y5MTAtukDc9q4IinNtPxAnRuI1-4E01hhQl-5bKKpwjq-u6wDNelm8P06K-fnp2STWIpdWR22oYyw0189FnZw`,
		want:  []string{},
	},
	{
		// { alg: RS256 }.{ scopes: { x: "a" } }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.eyJzY29wZXMiOnsieCI6ImEifX0.heSSwk-3KAfMsq-pyE5CBecwBBfPsqnWCkFVKFy9LSmjp26Dg-pCEItr5u2LFuYziJ1gQ73bpfIjmMWg2au6gEhcVRkz7_Ao36WYLWl8Q-WqaysTEajJs97Mx3voiCThVdC-2i6MjESOQZSFGYERGRXuXZbZaFahL3_xJW1jn0T4LHqJF_eNjCkkQCTNg-P3hfh59DB0Pou4Rl8xzJ4ylb0rwQAYCpUJN-i9Q6lxGLpWBrOYc3vGM3v2Pk_nYY3CHkEPRfxlO4fTyxCUi_vjOI7u7FEOpQgCoM2zOxcrbsfe6Ecs-x-A2RMzE_x5VDUndauAjclI4uqdUZBoG6A7qQ`,
		want:  []string{},
	},
	{
		// { alg: RS256 }.{ scopes: [ 1 ] } }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.eyJzY29wZXMiOlsxXX0.B7TInAaydbOBAFtmu2onpNGRl7uUejqMoCfobk0KJURC664r6RBrIzylBzorxhZNiJD6PSytJKxRwUd9TU9pHt7RZMNkdiSjpOmjeGtgcmHEt42D4K31TISonjetb2WFvspX74ELWxwE1dOv9iA2_0bua1layJJsA5zMHVrbl1bWWcsYUWYayXhiGvokVCJM-Uj3UM1235cogGwNnS878nGpACIz-6Zy-xjvOmag3N8kzeF6fxuE1AWOpyAS_eKfWycchkcv0wdJno72XjPq2bF0SE8nt71i7zPZ3XfK-s4Cf2OC5dE8MqO8hqyDWtF8SnMpbFgCwD_BoXLFsAnYCg`,
		want:  []string{},
	},
	{
		// { alg: RS256 }.{ scopes: [ "a", { x: 1 } ] } }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.eyJzY29wZXMiOlsiYSIseyJ4IjoxfV19.Sckms1Vgmba6oKj-l5zlBK2jJIY3Btilvk7oGV4WcnIrrHFrhJaPBigkhs15f9IYtuJW30RaJLt5VMB0J5jMUZPfNRR39WVGMOfTQlBsw7NWnYlxnP-NEa9bUdxfqd1NLeNmVvs563TOiRdXjr4ZImrB0V-q-c0RuaBbse5RAt2YXJu_BIq7nSY1yLO8o2gn8LdvJ04h24Cr_OO_XaGcGMUK84FMzn65iqtb-rQsrIsIIYWzT5gp2hOX-cw8ysc5vlUM6Hl71GfincNgTh5JKRVyxdvGQroIfHVLiw5Lcf3G3JoiAPVRsOhDbW4H9aRu6q1NmlUfkm0A-KNnuXdZUg`,
		want:  []string{"a"},
	},
	{
		// { alg: RS256 }.{ scopes: [] }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.eyJzY29wZXMiOltdfQ.NDoKBnYgN68veFdWH05nReGKt8g6eNNo0jD2xHAImWSPvyKzK1OtFOH_kDMGomenRQe8bzqxVngVa-Fh7FxcYwFDwRULGkUH617UVfx9FW5Bw9dby9Lfx4XdLs9zUSt6nvhJ0nohCYORM9Gvs2DDY-V8i_upaeE5FvLfYRTbTyBl1fmPaxWvAqNr_CGTpF2JUSaqUiE0pdvyocPrbcaZyq8tB26Gh6YWqV515bNvQCMBkW-fedW-2JBOjqGiwChom1q1qlGN5d9lw6DsBCf9nfWMdubR3QIbShSAwVdLUP-6lJNxKMS-PbcAgrWv6Ev14Yu1fQJwJ6omSNy8reEFHg`,
		want:  []string{},
	},
	{
		// { alg: RS256 }.{ scopes: [ "a" ] }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.eyJzY29wZXMiOlsiYSJdfQ.Ic1Krq6yc8Tt516zhbRus6bJh0CDXj5tQZgph_0CdW4zc9dtMOYOdVr9DJ3mv-J7kG7-pX7QZiDTcAK0vrnRfD98cB7aonRc97_TjvyO3H9HrdEabuxmfnnGgjyKQ4LCO-TCMj-mtbmmAVUyJ3gXpFuDLtOF0WvK2XIbVjTbtRpkEFEME0LqhpkVxIi_2I5V4oHF7yRLs_CMtxu1IZUY-ko9QLxr9aGWGM-AmG4yW1UcR93UBS5Mve2lY4-VDNrZYtTeTQ51vMbjxcQXcPH_ofQkoVl_TIGZNY33OvcHF0hUKCGPJWlsAIWHYBPV5iTW3RDqeci6owrZCmqI8RbJMw`,
		want:  []string{"a"},
	},
	{
		// { alg: RS256 }.{ scopes: [ "a", "b" ] }.sig
		token: `eyJhbGciOiJSUzI1NiJ9.eyJzY29wZXMiOlsiYSIsImIiXX0.AZy5YuwcZ0tR3vLCsyr8KuMA7H2vESWFM3QgBHRPxoXdrqtJm40n5ZoN01aY12xSnyVyhXJ7txNX7zaMH3Gjq7Q-1_RQCCHjYzGoxheQhBJk7l2KNXmnMRgHDfO41nEHpJ8JoGc1wc_e81Agl7kJ0jzt-_6bFiBkWx6AstJaFuoe8ZSqbE5w89k-99B_0Hnb6tBg_gohhma2w3lYS2oFR9q0vhTzLDy6y7GF3BhjWqcvN2MDhRqhWUin7tLpOgJ2Uc9W0zEjGOAcOXnWW-ljy5cBc0g4TRHJk_EWreolhNMlNKrpJOoL_Qwf_pChoZynQ0pBiIsTk8RqkeuSULbQug`,
		want:  []string{"a", "b"},
	},
}

func TestExtractScopesFromJwt(t *testing.T) {
	for k, d := range extractScopesFromJwtFixtures {
		got := extractScopesFromJwt(t, d.token)
		if !reflect.DeepEqual(d.want, got) {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}
