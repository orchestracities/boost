package jwt

import (
	"testing"
)

// all JWTs have invalid signatures to keep them short which is okay
// since we don't need to validate signatures here.

var missingSubjectCommonNameFixtures = []string{
	// { alg: HS256 }.{}.junk
	"eyJhbGciOiJIUzI1NiJ9.e30.Z",
	// { alg: HS256 }.{ sub: null }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOm51bGx9.Z",
	// { alg: HS256 }.{ sub: "" }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIifQ.Z",
	// { alg: HS256 }.{ sub: "C=DE,O=FIWARE,OU=CTOIDSA," }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJDPURFLE89RklXQVJFLE9VPUNUT0lEU0EsIn0.Z",
	// { alg: HS256 }.{ sub: "C=DE,O=FIWARE,OU=CTOIDSA,CN=" }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJDPURFLE89RklXQVJFLE9VPUNUT0lEU0EsQ049In0.Z",
}

func TestMissingSubjectCommonName(t *testing.T) {
	for k, jwt := range missingSubjectCommonNameFixtures {
		payload := FromRaw(jwt)
		got := payload.SubjectCommonName()
		if got != "" {
			t.Errorf("[%v] want: empty; got: %v", k, got)
		}
	}
}

var subjectCommonNameFixtures = []struct {
	jwt  string
	want string
}{
	// { alg: HS256 }.{ sub: "C=DE,O=FIWARE,OU=CTOIDSA,CN=4e16f007-d959-4eb2-b47d-78dd0c4eab0e" }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJDPURFLE89RklXQVJFLE9VPUNUT0lEU0EsQ049NGUxNmYwMDctZDk1OS00ZWIyLWI0N2QtNzhkZDBjNGVhYjBlIn0.Z",
		"4e16f007-d959-4eb2-b47d-78dd0c4eab0e"},
	// { alg: HS256 }.{ sub: "C=DE,O=FIWARE,OU=CTOIDSA,CN= xy23 " }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJDPURFLE89RklXQVJFLE9VPUNUT0lEU0EsQ049IHh5MjMgIn0.Z",
		"xy23"},
}

func TestSubjectCommonName(t *testing.T) {
	for k, d := range subjectCommonNameFixtures {
		payload := FromRaw(d.jwt)
		got := payload.SubjectCommonName()
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}
