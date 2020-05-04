package jwt

import (
	"fmt"
	"testing"
	"time"
)

var expiresInFixtures = []struct {
	exp interface{}
	lo  uint64
	hi  uint64
}{
	{-12.3, 0, 0}, {0, 0, 0}, {time.Now().Unix(), 0, 0},
	{float64(time.Now().Unix()) + 0.1, 0, 0},
	{time.Now().Unix() + 10, 5, 10},
	{"-12.3", 0, 0}, {"0", 0, 0}, {fmt.Sprintf("%v", time.Now().Unix()), 0, 0},
	{fmt.Sprintf("%v", float64(time.Now().Unix())+0.1), 0, 0},
	{fmt.Sprintf("%v", time.Now().Unix()+10), 5, 10},
}

func TestExpiresIn(t *testing.T) {
	for k, d := range expiresInFixtures {
		payload := Payload{
			"exp": d.exp,
		}
		got := payload.ExpiresIn()
		if got < d.lo || got > d.hi {
			t.Errorf("[%v] want: ∈ [%v, %v]; got: %v", k, d.lo, d.hi, got)
		}
	}
}

func TestExpiresInWhenMissingExp(t *testing.T) {
	payload := Payload{}
	got := payload.ExpiresIn()
	want := uint64(0)
	if got != want {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

func TestExpirationTimeWhenMissingExp(t *testing.T) {
	payload := Payload{}
	got := payload.ExpirationTime()
	want := uint64(0)
	if got != want {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

func TestExpiresInWhenNilExp(t *testing.T) {
	payload := Payload{"exp": nil}
	got := payload.ExpiresIn()
	want := uint64(0)
	if got != want {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

func TestExpirationTimeWhenNilExp(t *testing.T) {
	payload := Payload{"exp": nil}
	got := payload.ExpirationTime()
	want := uint64(0)
	if got != want {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

func TestExpiresInWhenExpHasWrongType(t *testing.T) {
	payload := Payload{"exp": "wada wada"}
	got := payload.ExpiresIn()
	want := uint64(0)
	if got != want {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

func TestExpirationTimeWhenExpHasWrongType(t *testing.T) {
	payload := Payload{"exp": " 123 "}
	got := payload.ExpirationTime()
	want := uint64(0)
	if got != want {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

var isWithinAllowedTimeIntervalFixtures = []struct {
	payload Payload
	want    bool
}{
	// cover all cases in case analysis table
	// (0, 0)
	{Payload{}, true},
	{Payload{"nbf": nil}, true},
	{Payload{"exp": nil}, true},
	{Payload{"nbf": nil, "exp": nil}, true},
	// (0, 1)
	{Payload{"exp": "wada wada"}, false},
	{Payload{"exp": []int{123}}, false},
	{Payload{"exp": "2578569936 "}, false}, // spaces not allowed in num-str
	//               ^ ~= 17 Sep 2051 @ 15:25pm
	{Payload{"exp": " 2578569936"}, false},
	// (0, 2)
	{Payload{"exp": 123000}, false}, // in the past
	{Payload{"exp": 2578569936}, true},
	//               ^ ~= 17 Sep 2051 @ 15:25pm
	{Payload{"exp": "2578569936"}, true},
	// (1, 0)
	{Payload{"nbf": "wada wada"}, false},
	{Payload{"nbf": []int{123}}, false},
	{Payload{"nbf": "2578569936 "}, false}, // spaces not allowed in num-str
	//               ^ ~= 17 Sep 2051 @ 15:25pm
	{Payload{"nbf": " 2578569936"}, false},
	// (1, 1)
	{Payload{"nbf": "1 2", "exp": " 32"}, false},
	{Payload{"nbf": []int{123}, "exp": []int{}}, false},
	// (1, 2)
	{Payload{"nbf": "1 2", "exp": 2578569936}, false},
	//                            ^ ~= 17 Sep 2051 @ 15:25pm
	{Payload{"nbf": []int{123}, "exp": 2578569936}, false},
	// (2, 0)
	{Payload{"nbf": 123000}, true}, // in the past
	{Payload{"nbf": 2578569936}, false},
	//               ^ ~= 17 Sep 2051 @ 15:25pm
	// (2, 1)
	{Payload{"nbf": 123000, "exp": "x"}, false},
	{Payload{"nbf": 2578569936, "exp": " 2578569936 "}, false},
	// (2, 2)
	{Payload{"nbf": 123000, "exp": "2578569936"}, true},
	{Payload{"nbf": 123000, "exp": 2578569936}, true},
	{Payload{"nbf": "123000", "exp": "2578569936"}, true},
	{Payload{"nbf": "123000", "exp": 2578569936}, true},
	{Payload{"nbf": 2578569936, "exp": 3578569936}, false}, // now < nbf < exp
	{Payload{"nbf": 123000, "exp": 223000}, false},         //  nbf < exp < now
	{Payload{"nbf": 223000, "exp": 123000}, false},         //  exp < nbf < now
}

func TestIsWithinAllowedTimeInterval(t *testing.T) {
	for k, d := range isWithinAllowedTimeIntervalFixtures {
		got := d.payload.IsWithinAllowedTimeInterval()
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}
