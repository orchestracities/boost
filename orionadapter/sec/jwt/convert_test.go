package jwt

import (
	"fmt"
	"math"
	"reflect"
	"testing"
)

var junkToMapFixtures = []interface{}{
	nil, -123, false, []string{"a", "b"}, struct{ x int }{x: 3},
}

func TestJunkToMap(t *testing.T) {
	for k, x := range junkToMapFixtures {
		got := toMap(x)
		if len(got) != 0 {
			t.Errorf("[%v] want empty map; got: %v", k, got)
		}
	}
}

var toMapFixtures = []interface{}{
	map[string]interface{}{"x": 1},
	map[string]interface{}{"x": 1, "t": "hi!"},
	map[string]interface{}{"x": 1, "w": []string{"a", "b"}},
}

func TestToMap(t *testing.T) {
	for k, want := range toMapFixtures {
		got := toMap(want)
		if !reflect.DeepEqual(want, got) {
			t.Errorf("[%v] want: %v; got: %v", k, want, got)
		}
	}
}

var toMapOfStringFixtures = []struct {
	input interface{}
	want  map[string]string
}{
	{
		input: nil,
		want:  map[string]string{},
	},
	{
		input: map[string]interface{}{},
		want:  map[string]string{},
	},
	{
		input: map[string]interface{}{"x": 1},
		want:  map[string]string{"x": "1"},
	},
	{
		input: map[string]interface{}{"x": 1, "t": "hi!"},
		want:  map[string]string{"x": "1", "t": "hi!"},
	},
	{
		input: map[string]interface{}{"x": 1, "w": []string{"a", "b"}},
		want:  map[string]string{"x": "1", "w": "[a b]"},
	},
}

func TestToMapOfString(t *testing.T) {
	for k, d := range toMapOfStringFixtures {
		got := toMapOfString(d.input)
		if !reflect.DeepEqual(d.want, got) {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}

var junkToListOfMapFixtures = []interface{}{
	nil, -123, false, []string{"a", "b"}, struct{ x int }{x: 3},
	map[string]interface{}{"x": 1},
}

func TestJunkToListOfMap(t *testing.T) {
	for k, x := range junkToListOfMapFixtures {
		got := toListOfMap(x)
		if len(got) != 0 {
			t.Errorf("[%v] want empty array; got: %v", k, got)
		}
	}
}

func TestEmptyArrayToListOfMap(t *testing.T) {
	empty := []interface{}{}
	got := toListOfMap(empty)
	if len(got) != 0 {
		t.Errorf("want empty array; got: %v", got)
	}
}

func TestSingletonArrayToListOfMap(t *testing.T) {
	xs := []interface{}{
		map[string]interface{}{"x": 1},
	}
	got := toListOfMap(xs)
	if len(got) != 1 || got[0]["x"] != 1 {
		t.Errorf("want: %v; got: %v", xs, got)
	}
}

func TestMultiEntryArrayToListOfMap(t *testing.T) {
	xs := []interface{}{
		map[string]interface{}{"x": 1},
		map[string]interface{}{"x": 1, "y": "2"},
	}
	got := toListOfMap(xs)
	if len(got) != 2 || got[0]["x"] != 1 {
		t.Errorf("want: %v; got: %v", xs, got)
	}
	if got[1]["x"] != 1 || got[1]["y"] != "2" {
		t.Errorf("want: %v; got: %v", xs, got)
	}
}

func TestToListOfMapFilterOutJunk(t *testing.T) {
	xs := []interface{}{
		map[string]interface{}{"x": 1},
		"junk",
	}
	want := []map[string]interface{}{
		{"x": 1},
	}
	got := toListOfMap(xs)
	if !reflect.DeepEqual(want, got) {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

var junkToListOfStringFixtures = []interface{}{
	nil, -123, false, []int{1, 2}, struct{ x string }{x: "3"},
}

func TestJunkToListOfString(t *testing.T) {
	for k, x := range junkToListOfStringFixtures {
		got := toListOfString(x)
		if len(got) != 0 {
			t.Errorf("[%v] want empty slice; got: %v", k, got)
		}
	}
}

func TestEmptyArrayToListOfString(t *testing.T) {
	empty := []interface{}{}
	got := toListOfString(empty)
	if len(got) != 0 {
		t.Errorf("want empty array; got: %v", got)
	}
}

func TestSingletonArrayToListOfString(t *testing.T) {
	x := []interface{}{"x"}
	want := []string{"x"}
	got := toListOfString(x)
	if !reflect.DeepEqual(want, got) {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

func TestMultiEntryArrayToListOfString(t *testing.T) {
	x := []interface{}{"x", "y"}
	want := []string{"x", "y"}
	got := toListOfString(x)
	if !reflect.DeepEqual(want, got) {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

func TestToListOfStringFilterOutJunk(t *testing.T) {
	xs := []interface{}{
		[]string{"junk"},
		"x",
	}
	want := []string{"x"}
	got := toListOfString(xs)
	if !reflect.DeepEqual(want, got) {
		t.Errorf("want: %v; got: %v", want, got)
	}
}

var collectStringsFixtures = []struct {
	input []map[string]interface{}
	want  []string
}{
	{input: nil, want: []string{}},
	{input: []map[string]interface{}{}, want: []string{}},
	{
		input: []map[string]interface{}{
			{"x": 1},
		},
		want: []string{},
	},
	{
		input: []map[string]interface{}{
			{"x": 1}, {"k": nil},
		},
		want: []string{},
	},
	{
		input: []map[string]interface{}{
			{"x": 1, "k": nil}, {"x": 1, "k": 2},
		},
		want: []string{},
	},
	{
		input: []map[string]interface{}{
			{"x": 1, "k": nil}, {"x": 1, "k": 2}, {"x": 1, "k": "a"},
		},
		want: []string{"a"},
	},
	{
		input: []map[string]interface{}{
			{"k": "b"}, {"x": 1, "k": nil}, {"x": 1, "k": 2},
			{"x": 1, "k": "a"},
		},
		want: []string{"b", "a"},
	},
	{
		input: []map[string]interface{}{
			{"k": "b"}, {"x": 1, "k": nil}, {"x": 1, "k": 2},
			{"x": 1, "k": "a"}, {"x": 1, "k": "b"}, {"k": ""},
		},
		want: []string{"b", "a", "b", ""},
	},
}

func TestCollectStrings(t *testing.T) {
	for k, d := range collectStringsFixtures {
		got := collectStrings("k", d.input)
		if !reflect.DeepEqual(d.want, got) {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}

var dedupeStringsFixtures = []struct {
	input []string
	want  []string
}{
	{nil, []string{}}, {[]string{}, []string{}},
	{[]string{"a"}, []string{"a"}},
	{[]string{"a", ""}, []string{"", "a"}},
	{[]string{"a", "b", "a"}, []string{"a", "b"}},
	{[]string{"a", "", "", "a"}, []string{"", "a"}},
}

func TestDedupeStrings(t *testing.T) {
	for k, d := range dedupeStringsFixtures {
		got := dedupeStrings(d.input)
		if !reflect.DeepEqual(d.want, got) {
			t.Errorf("[%v] want: %v; got (sorted): %v", k, d.want, got)
		}
	}
}

var intToUint64Fixtures = []struct {
	input int64
	want  uint64
}{
	{0, 0}, {-123, 0}, {math.MinInt64, 0}, {1, 1}, {123, 123},
	{math.MaxInt64, math.MaxInt64},
}

func TestIntToUint64(t *testing.T) {
	for k, d := range intToUint64Fixtures {
		got := intToUint64(d.input)
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}

var floatToUint64Fixtures = []struct {
	input float64
	want  uint64
}{
	{0, 0}, {-123, 0}, {math.MinInt64, 0}, {1, 1}, {123, 123},
	{math.MaxUint64, math.MaxUint64},
	{0.0, 0}, {-1.23, 0}, {1.1, 1}, {123.9, 123},
}

func TestFloatToUint64(t *testing.T) {
	for k, d := range floatToUint64Fixtures {
		var got uint64 = floatToUint64(d.input)
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}

var stringToUint64Fixtures = []struct {
	input string
	want  uint64
}{
	{"", 0}, {"\n", 0}, {"junk", 0},
	{"0", 0}, {"-123", 0},
	{fmt.Sprintf("%d", int64(math.MinInt64)), 0},
	{"1", 1}, {"123", 123},
	{fmt.Sprintf("%d", uint64(math.MaxUint64)), math.MaxUint64},
	{"0.0", 0}, {"-1.23", 0}, {"1.1", 1}, {"123.9", 123},
}

func TestStringToUint64(t *testing.T) {
	for k, d := range stringToUint64Fixtures {
		var got uint64 = stringToUint64(d.input)
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}

var toUnit64Fixtures = []struct {
	input interface{}
	want  uint64
}{
	{int8(123), 123}, {int16(123), 123}, {int32(123), 123},
	{int64(123), 123}, {int(123), 123},
	{uint8(123), 123}, {uint16(123), 123}, {uint32(123), 123},
	{uint64(123), 123}, {uint(123), 123},
	{float32(123.9), 123}, {float64(123.9), 123},
	{"123", 123},
	{stringToUint64Fixtures, 0},
}

func TestToUint64(t *testing.T) {
	for k, d := range toUnit64Fixtures {
		var got uint64 = toUint64(d.input)
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}
