package jwt

import (
	"math"
	"sort"
	"strconv"
	"time"
)

// safe conversion functions

func toMap(x interface{}) map[string]interface{} {
	switch x.(type) {
	case map[string]interface{}:
		return x.(map[string]interface{})
	default:
		return map[string]interface{}{}
		// NOTE. Map zero value. Behaves like empty map when queried but
		// bombs out on write. Good enough for us though since we're not
		// supposed to write to it.
	}
}

func toListOfMap(x interface{}) []map[string]interface{} {
	switch x.(type) {
	case []interface{}:
		raw := x.([]interface{})
		var ms = make([]map[string]interface{}, 0, len(raw))
		for _, r := range raw {
			m := toMap(r)
			if len(m) != 0 {
				ms = append(ms, m)
			}
		}
		return ms
	default:
		return []map[string]interface{}{}
		// NOTE. Array zero value. Behaves like empty array when queried but
		// bombs out on write. Good enough for us though since we're not
		// supposed to write to it.
	}
}

func toListOfString(x interface{}) []string {
	switch x.(type) {
	case []interface{}:
		xs := x.([]interface{})
		return filterStrings(xs)
	default:
		return []string{}
	}
}

func filterStrings(xs []interface{}) []string {
	size := len(xs)
	ys := make([]string, 0, size)
	for _, x := range xs {
		if y, ok := x.(string); ok {
			ys = append(ys, y)
		}
	}
	return ys
}

func collectStrings(key string, ms []map[string]interface{}) []string {
	var vs = make([]interface{}, len(ms), len(ms))
	for k, m := range ms {
		vs[k] = m[key]
	}
	return filterStrings(vs)
}

func dedupeStrings(xs []string) []string {
	keySet := make(map[string]bool)
	for _, x := range xs {
		keySet[x] = true
	}

	ks := make([]string, 0, len(keySet))
	for k := range keySet {
		ks = append(ks, k)
	}

	sort.Sort(sort.StringSlice(ks))
	return ks
}

func isNumeric(x interface{}) bool {
	switch x.(type) {
	case int8, int16, int32, int64, int, uint8, uint16, uint32, uint64,
		uint, float32, float64: // NB in Go, byte = uint8
		return true
	case string:
		_, err := strconv.ParseFloat(x.(string), 64)
		return err == nil
	default:
		return false
	}
}

func toUint64(x interface{}) uint64 {
	switch x.(type) {
	case int8:
		return intToUint64(int64(x.(int8)))
	case int16:
		return intToUint64(int64(x.(int16)))
	case int32:
		return intToUint64(int64(x.(int32)))
	case int64:
		return intToUint64(x.(int64))
	case int:
		return intToUint64(int64(x.(int)))
	case uint8: // NB in Go, byte = uint8
		return uint64(x.(uint8))
	case uint16:
		return uint64(x.(uint16))
	case uint32:
		return uint64(x.(uint32))
	case uint64:
		return x.(uint64)
	case uint:
		return uint64(x.(uint))
	case float32:
		return floatToUint64(float64(x.(float32)))
	case float64:
		return floatToUint64(x.(float64))
	case string:
		return stringToUint64(x.(string))
	default:
		return 0
	}
}

func intToUint64(x int64) uint64 {
	if x <= 0 {
		return 0
	}
	return uint64(x)
}

func floatToUint64(x float64) uint64 {
	y := math.Floor(x)
	if y <= 0 {
		return 0
	}
	if y >= math.MaxUint64 {
		return math.MaxUint64
	}
	return uint64(y)
}

func stringToUint64(x string) uint64 {
	y, err := strconv.ParseFloat(x, 64)
	if err != nil {
		return 0
	}
	return floatToUint64(y)
}

func toString(x interface{}) string {
	switch x.(type) {
	case string:
		return x.(string)
	default:
		return ""
	}
}

func secondsSinceEpoch() uint64 {
	return toUint64(time.Now().Unix())
}
