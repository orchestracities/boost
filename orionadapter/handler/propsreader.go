package handler

import (
	"fmt"

	ipolicy "istio.io/api/policy/v1beta1"
)

func cast(value interface{}) interface{} {
	switch t := value.(type) {
	case *ipolicy.Value_StringValue:
		return t.StringValue
	case *ipolicy.Value_Int64Value:
		return t.Int64Value
	case *ipolicy.Value_DoubleValue:
		return t.DoubleValue
	default:
		return fmt.Sprintf("%v", value)
	}
}

func toMap(in map[string]*ipolicy.Value) map[string]interface{} {
	out := make(map[string]interface{}, len(in))
	for k, v := range in {
		out[k] = cast(v.GetValue())
	}
	return out
}

func getStringValue(m map[string]interface{}, key string) string {
	value := ""

	if v, exists := m[key]; exists {
		value = fmt.Sprintf("%v", v)
	}

	return value
}
