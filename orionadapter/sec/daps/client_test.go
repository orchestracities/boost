package daps

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"
	"testing"
)

var baseURLJoin = []struct {
	host string
	rest string
	want string
}{
	{host: "", rest: "", want: "https:///"},
	{host: "", rest: "p", want: "https:///p"},
	{host: "", rest: "/p", want: "https:///p"},
	{host: "", rest: "p/", want: "https:///p/"},
	{host: "", rest: "/p/", want: "https:///p/"},
	{host: "", rest: "p/?q", want: "https:///p/?q"},
	{host: "", rest: "/p/?q", want: "https:///p/?q"},
	{host: "", rest: "p/?q=1", want: "https:///p/?q=1"},
	{host: "", rest: "/p/?q=1", want: "https:///p/?q=1"},
	{host: "", rest: "p/?q=1&qq=2", want: "https:///p/?q=1&qq=2"},
	{host: "", rest: "/p/?q=1&qq=2", want: "https:///p/?q=1&qq=2"},
	{host: "h", rest: "", want: "https://h/"},
	{host: "h", rest: "p", want: "https://h/p"},
	{host: "h", rest: "/p", want: "https://h/p"},
	{host: "h", rest: "p/", want: "https://h/p/"},
	{host: "h", rest: "/p/", want: "https://h/p/"},
	{host: "h", rest: "p/?q", want: "https://h/p/?q"},
	{host: "h", rest: "/p/?q", want: "https://h/p/?q"},
	{host: "h", rest: "p/?q=1", want: "https://h/p/?q=1"},
	{host: "h", rest: "/p/?q=1", want: "https://h/p/?q=1"},
	{host: "h", rest: "p/?q=1&qq=2", want: "https://h/p/?q=1&qq=2"},
	{host: "h", rest: "/p/?q=1&qq=2", want: "https://h/p/?q=1&qq=2"},
	{host: "h:8080", rest: "", want: "https://h:8080/"},
	{host: "h:8080", rest: "p", want: "https://h:8080/p"},
	{host: "h:8080", rest: "/p", want: "https://h:8080/p"},
	{host: "h:8080", rest: "p/", want: "https://h:8080/p/"},
	{host: "h:8080", rest: "/p/", want: "https://h:8080/p/"},
	{host: "h:8080", rest: "p/?q", want: "https://h:8080/p/?q"},
	{host: "h:8080", rest: "/p/?q", want: "https://h:8080/p/?q"},
	{host: "h:8080", rest: "p/?q=1", want: "https://h:8080/p/?q=1"},
	{host: "h:8080", rest: "/p/?q=1", want: "https://h:8080/p/?q=1"},
	{host: "h:8080", rest: "p/?q=1&qq=2", want: "https://h:8080/p/?q=1&qq=2"},
	{host: "h:8080", rest: "/p/?q=1&qq=2", want: "https://h:8080/p/?q=1&qq=2"},
}

func TestBaseURLJoin(t *testing.T) {
	for _, k := range baseURLJoin {
		u := &baseURL{
			scheme: "https",
			host:   k.host,
		}
		got := u.join(k.rest)
		if k.want != got {
			t.Errorf("want: %s; got: %s", k.want, got)
		}
	}
}

// Below cert and associated private key got generated with:
//
// openssl req -newkey rsa:2048 \
//   -new -nodes -x509 \
//   -days 3650 \
//   -out cert.pem \
//   -keyout key.pem \
//   -subj "/C=ZA/ST=Western Cape/L=Cape Town/O=Rusks Ltd/OU=Ma/CN=localhost"

const testCert = `-----BEGIN CERTIFICATE-----
MIIDVjCCAj4CCQDp28MTQXuiwjANBgkqhkiG9w0BAQsFADBtMQswCQYDVQQGEwJa
QTEVMBMGA1UECAwMV2VzdGVybiBDYXBlMRIwEAYDVQQHDAlDYXBlIFRvd24xEjAQ
BgNVBAoMCVJ1c2tzIEx0ZDELMAkGA1UECwwCTWExEjAQBgNVBAMMCWxvY2FsaG9z
dDAeFw0yMDAxMTQxNjI0MzVaFw0zMDAxMTExNjI0MzVaMG0xCzAJBgNVBAYTAlpB
MRUwEwYDVQQIDAxXZXN0ZXJuIENhcGUxEjAQBgNVBAcMCUNhcGUgVG93bjESMBAG
A1UECgwJUnVza3MgTHRkMQswCQYDVQQLDAJNYTESMBAGA1UEAwwJbG9jYWxob3N0
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtS/5beoFSmSDQe+pMAWL
+hGF/Y2BpF9rQbgit+gdBlVFwE3xH7DulNG4CZxfNlLRPDa9fQPFbFbmD0xsnFLU
d2dolMAKsAYoZjww913Xiqxc0ttyhH9TnMKZW1RbFAYsqTDbsYovSj9iDnhXMzXj
b9Q1BkjStnuF5IWxhcpQlvmHr9/acWCyOIU/BfCTRLg8F+30JP3Dt3DcxPwPU5Js
fUrH3P3KCnQtNk/+dyppZamqKCa3C0yVtpMDPcea/rtVNwP/Cu7iQNnjYpML92kW
xXR/tm5fk45J/mpC+OK2VSCRpeSb50UMKBC/lb6NGa0PohS4kgNkfZXXPzSyWuhX
gwIDAQABMA0GCSqGSIb3DQEBCwUAA4IBAQBTqJskNcwTM1WxJrC14oWLuekvA4fE
Ugu2RhLLjhmK16mRGn9QE2/N7lE1TdJ8xX0NhyYeKR9dosOxMIRDNI3z+gKw8qJO
/UrKavgcbZsJH+OQiNZ64iIgeq6AsdEcDD8bs2QjdldttgLTALZ8d66bbLhjwn0j
GZpXKhhs/24Oi/vlDOyXh538HNXq5UddkRcRLdl9arocEwNDy4NpsbVPXeVIrok0
0tsc6KvwtAjsjWpEjgOI0Pl0+l4hA0MoMLxV15onKeCi7JQaLaP8tmNlXHFj9qu+
X5UpiCO6e/pce8I5RwyQEKjpcyDNyWaUk1Z1xwYEzUcxtvvYxpUpyOOx
-----END CERTIFICATE-----
`
const testCertPvtKey = `-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC1L/lt6gVKZINB
76kwBYv6EYX9jYGkX2tBuCK36B0GVUXATfEfsO6U0bgJnF82UtE8Nr19A8VsVuYP
TGycUtR3Z2iUwAqwBihmPDD3XdeKrFzS23KEf1OcwplbVFsUBiypMNuxii9KP2IO
eFczNeNv1DUGSNK2e4XkhbGFylCW+Yev39pxYLI4hT8F8JNEuDwX7fQk/cO3cNzE
/A9Tkmx9Ssfc/coKdC02T/53KmllqaooJrcLTJW2kwM9x5r+u1U3A/8K7uJA2eNi
kwv3aRbFdH+2bl+Tjkn+akL44rZVIJGl5JvnRQwoEL+Vvo0ZrQ+iFLiSA2R9ldc/
NLJa6FeDAgMBAAECggEACYp0LPiEvM0cKE2xcAjqvQlNL/PSntAzeqtykJKVbK7y
1FSGXO/ZMFb2xPLKBLdJs00Cn2GidLkCtk2E7pph+8OjOyn9phU87V1ACtaTMgcv
gB70Icv+oCOTJb8EaMKGeYZMG0Y2hUdfJ3noxZaR2mKnRCRzjA5nF4h+t5fWtIxq
doDh6j33i0d0l+3K9nR1EP4K46FkUNSjVmbJhOTYU9q8H4ewZ2RzgknGcRWNhV5z
M4/X37FfVO85Jn2kWaPx3NxFtGUXqzbCsYPnNaY/lhhfJdXNNlu9Qi1NyAdEDJFU
wwYRi66y6CABttojCzilDRv73BFlbwf611ZG5qACmQKBgQDrP48pe1RTlLe4Hac+
SihKW2G0mFkgkxj9dp3VnbduxOXbKwrz5GaIZaD+o+BeYmqjRiryKBhvcq7xZb00
r6ko/xhFcPeomvYCrNg1sm1FVyLD02I5xNodolDISH9VagZofQ1S8sJzeytiZNzJ
metSJ6rpy7LDUtrkbq3Ss7zgnQKBgQDFK5j8sOuF8A+0zBGhOAcKetCm+4YBHQCy
WWluxhL7S2rJXXhLM2qssp56FjOFavQ1sLlFLnrEsaNgklNU7fIWy2ImmT0sa8Y2
S3UzeXhZ5jMUOLsQW23zsP7Z6c0U0hMpzQRYVEKbutk1z6Qga/k1JqjwOgTaZztK
si2Kpf1OnwKBgB+WiUKorMoMTh8K3Eog6wgQ/S2ix1T4a4KdStREOT1Gcxba0L2v
DZWDD/shRh9mV6tU4K9jcuSEIbmIT7+jVrOKjVfFs3uQUzhIvT94lfOZn7Fr0OSw
6hjQkshR88ckVXfyUrewoSugflLX+E2ZvV9qtChwkbBoj7vcoLqKJ/KBAoGBAIA6
C0OC15kCd2RwNqLvafzRxHJkL1D4CKT0axHkdSHCeU89n2bgqGZpv5DMcXM6DFoC
dWrdgG/8yrCaWOFp4cAbQtixXcxOxtg2mKECRVfJ0rw67MUFgOsz13nmiD4bJOVR
dJrxKWRXzr0lLar8LVT4sHOSd+eFrVS1rdJ2gtcnAoGAYwcT3uVWoPNcZHrnaYDi
Eo1+9kd8vS5mr5bgIOJ1nS9np4Bg6qDyTtgVZHanIjXw/1UZ4tRKbqd0mC+ZfFNP
N6kAg1/mvAdO45ZwCd2DJbNC8/U5e7lAxcqJBKp/i0VwmeAcZyfXDttD6epNsuOM
wSWFX8pQTNWseFTkRvJEShM=
-----END PRIVATE KEY-----`

func TestBuildClientWithValidParams(t *testing.T) {
	host, clPvtKey, clCert, srvCert :=
		"whoopsie.dapsie", testCertPvtKey, testCert, testCert
	_, err := NewClient(host, clPvtKey, clCert, srvCert)
	if err != nil {
		t.Errorf("should've built a client struct: %v", err)
	}
}

var cantBuildClientWithoutClientPvtKey = []struct{ pvtK string }{
	{""}, {" "}, {"junk"},
	{`-----BEGIN PRIVATE KEY-----
-----END PRIVATE KEY-----`},
	{`-----BEGIN PRIVATE KEY-----
	MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC1L
-----END PRIVATE KEY-----`},
}

func TestCantBuildClientWithoutClientPvtKey(t *testing.T) {
	host, clCert, srvCert :=
		"whoopsie.dapsie", testCert, testCert
	for i, k := range cantBuildClientWithoutClientPvtKey {
		_, err := NewClient(host, k.pvtK, clCert, srvCert)
		if err == nil {
			t.Errorf("[%d] shouldn't have built a client struct: %v", i, err)
		}
	}
}

var cantBuildClientWithoutClientCert = []struct{ cert string }{
	{""}, {" "}, {"junk"},
	{`-----BEGIN CERTIFICATE-----
-----END CERTIFICATE-----`},
	{`-----BEGIN CERTIFICATE-----
	MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC1L
-----END CERTIFICATE-----`},
}

func TestCantBuildClientWithoutClientCert(t *testing.T) {
	host, clPvtKey, srvCert :=
		"whoopsie.dapsie", testCertPvtKey, testCert
	for i, k := range cantBuildClientWithoutClientCert {
		_, err := NewClient(host, clPvtKey, k.cert, srvCert)
		if err == nil {
			t.Errorf("[%d] shouldn't have built a client struct: %v", i, err)
		}
	}
}

var cantBuildClientWithoutServerCert = []struct{ cert string }{
	{""}, {" "}, {"junk"},
	{`-----BEGIN CERTIFICATE-----
-----END CERTIFICATE-----`},
	{`-----BEGIN CERTIFICATE-----
	MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC1L
-----END CERTIFICATE-----`},
}

func TestCantBuildClientWithoutServerCert(t *testing.T) {
	host, clPvtKey, clCert :=
		"whoopsie.dapsie", testCertPvtKey, testCert
	for i, k := range cantBuildClientWithoutServerCert {
		_, err := NewClient(host, clPvtKey, clCert, k.cert)
		if err == nil {
			t.Errorf("[%d] shouldn't have built a client struct: %v", i, err)
		}
	}
}

var responseBodyAsString = []struct {
	body []byte
	want string
}{
	{body: []byte{}, want: ""},
	{body: []byte{50}, want: "2"},
	{body: []byte{13, 10}, want: "\r\n"},
	{body: []byte{48, 49, 50}, want: "012"},
}

func TestResponseBodyAsString(t *testing.T) {
	for i, k := range responseBodyAsString {
		got := (ResponseBody(k.body)).AsString()
		if got != k.want {
			t.Errorf("[%d] want: %s; got: %s", i, k.want, got)
		}
	}
}

func TestResponseBodyAsJSON(t *testing.T) {
	type X struct {
		V int `json:"v"`
	}
	var x X
	body := ResponseBody([]byte(`{"v": 1}`))
	body.AsJSON(&x)
	if x.V != 1 {
		t.Errorf("want: v=1; got: %v", x)
	}
}

func TestResponseBodyAsJSONError(t *testing.T) {
	type X struct {
		V int `json:"v"`
	}
	var x X
	body := ResponseBody([]byte(``))
	err := body.AsJSON(&x)
	if err == nil {
		t.Errorf("should return parse error instead of: %v", x)
	}
}

func TestResponseBodyAsJSONMap(t *testing.T) {
	body := ResponseBody([]byte(`{"v": 1}`))
	m, _ := body.AsJSONMap()
	if m["v"].(float64) != 1 {
		t.Errorf("want: v=1; got: %v", m)
	}
}

func TestResponseBodyAsJSONMapError(t *testing.T) {
	body := ResponseBody([]byte(``))
	m, err := body.AsJSONMap()
	if err == nil {
		t.Errorf("should return parse error instead of: %v", m)
	}
}

func successCodes() []int {
	return []int{200, 201, 202, 203, 204}
}

func failureCodes() []int {
	codes := make([]int, 0, 300)
	for i := 100; i < 200; i++ {
		codes = append(codes, i)
	}
	for i := 205; i < 599; i++ {
		codes = append(codes, i)
	}
	return codes
}

func TestHandleResponseAcceptEmpty(t *testing.T) {
	for _, code := range successCodes() {
		r := &http.Response{
			StatusCode: code,
			Body:       ioutil.NopCloser(strings.NewReader("")),
		}
		payload, err := handleResponse(false, r, nil)
		if err != nil {
			t.Errorf("should've accepted empty %d: %v", code, err)
		}
		if !payload.IsEmpty() {
			t.Errorf("should've returned empty payload: %v", payload)
		}
	}
}

func TestHandleResponseRejectEmpty(t *testing.T) {
	for _, code := range successCodes() {
		r := &http.Response{
			StatusCode: code,
			Body:       ioutil.NopCloser(strings.NewReader("")),
		}
		payload, err := handleResponse(true, r, nil)
		if err == nil {
			t.Errorf("should've rejected empty %d", code)
		}
		if !payload.IsEmpty() {
			t.Errorf("should've returned empty payload: %v", payload)
		}
	}
}

func TestHandleResponseReadContent(t *testing.T) {
	content := "TestHandleResponseReadContent"
	for _, code := range successCodes() {
		r := &http.Response{
			StatusCode:    code,
			Body:          ioutil.NopCloser(strings.NewReader(content)),
			ContentLength: int64(len(content)),
		}
		payload, err := handleResponse(true, r, nil)
		if err != nil {
			t.Errorf("should've read the body of a %d: %v", code, err)
		}
		if payload.IsEmpty() {
			t.Errorf("should've returned the %d body content", code)
		}
		got := payload.AsString()
		if got != content {
			t.Errorf("should've returned the %d body content but got: %s",
				code, got)
		}
	}
}

func assertHandleResponseWithFailureCode(t *testing.T, r *http.Response,
	ensureContent bool) {
	payload, err := handleResponse(ensureContent, r, nil)
	if err == nil {
		t.Errorf("should've rejected %d: %v", r.StatusCode, err)
	}
	if payload != nil {
		t.Errorf("should've returned nil payload: %v", payload)
	}
	if !strings.Contains(err.Error(), r.Status) {
		t.Errorf("should've added status line to error: %v", err)
	}
}

func TestHandleResponseWithFailureCode(t *testing.T) {
	for _, code := range failureCodes() {
		r := &http.Response{
			StatusCode: code,
			Status:     "TestHandleResponseWithFailureCode",
		}
		assertHandleResponseWithFailureCode(t, r, false)
		assertHandleResponseWithFailureCode(t, r, true)
	}
}

func assertHandleResponseReturnInputError(t *testing.T, ensureContent bool) {
	var r http.Response
	prevErr := fmt.Errorf("TestHandleResponseReturnInputError")
	payload, err := handleResponse(ensureContent, &r, prevErr)
	if err != prevErr {
		t.Errorf("should've returned error as is: %v", err)
	}
	if payload != nil {
		t.Errorf("should've returned nil payload: %v", payload)
	}
}

func TestHandleResponseReturnInputError(t *testing.T) {
	assertHandleResponseReturnInputError(t, false)
	assertHandleResponseReturnInputError(t, true)
}
