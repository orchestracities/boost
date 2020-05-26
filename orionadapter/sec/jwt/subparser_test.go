package jwt

import (
	"testing"
)

var noCnMatchFixtures = []string{
	"", " ", "C N=123", "C=DE,O=FIWARE,OU=CTOIDSA,",
	"C=DE,O=FIWARE,OU=CTOIDSA,CN=", "C=DE,O=FIWARE,OU=CTOIDSA,CN= ",
	"C=DE,O=FIWARE,OU=CTOIDSA,CN=", "C=DE,O=FIWARE,OU=CTOIDSA,CN= ,",
}

func TestNoCnMatch(t *testing.T) {
	for k, input := range noCnMatchFixtures {
		value, ok := parseSubjectCommonName(input)
		if ok || value != "" {
			t.Errorf("[%v] shouldn't match: %s\ngot: %s", k, input, value)
		}
	}
}

var cnMatchFixtures = []struct {
	input string
	want  string
}{
	{"CN=1", "1"}, {"CN=12-34-56", "12-34-56"},
	{" CN=1", "1"}, {" CN=12-34-56", "12-34-56"},
	{" CN=1 ", "1"}, {" CN=12-34-56 ", "12-34-56"},
	{",CN=1", "1"}, {",CN=12-34-56", "12-34-56"},
	{" , CN=1", "1"}, {" , CN=12-34-56", "12-34-56"},
	{" , CN=1 ", "1"}, {" , CN=12-34-56 ", "12-34-56"},
	{" , CN=1,", "1"}, {" , CN=12-34-56,", "12-34-56"},
	{"CN=1 ,", "1"}, {"CN=12-34-56 ,", "12-34-56"},
	{" , CN   =  1  ", "1"}, {"CN  =  12-34-56  , ", "12-34-56"},
	{" , CN   =  x  ", "x"}, {"CN  =  wada-wada!  , ", "wada-wada!"},
	{"C=DE,O=FIWARE,OU=CTOIDSA,CN=4e16f007-d959-4eb2-b47d-78dd0c4eab0e",
		"4e16f007-d959-4eb2-b47d-78dd0c4eab0e"},
	{"C =DE, O= FIWARE ,OU = CTOI DSA, CN = 4e16f007-d959-4eb2-b47d-78dd0c4eab0e, A=x",
		"4e16f007-d959-4eb2-b47d-78dd0c4eab0e"},
}

func TestCnMatch(t *testing.T) {
	for k, d := range cnMatchFixtures {
		got, ok := parseSubjectCommonName(d.input)
		if !ok {
			t.Errorf("[%v] should match: %s\ngot: %s", k, d.input, got)
		}
		if d.want != got {
			t.Errorf("[%v] want: %s; got: %s", k, d.want, got)
		}
	}
}
