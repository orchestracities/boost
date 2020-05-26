package jwt

import (
	"reflect"
	"testing"
)

// all JWTs have invalid signatures to keep them short which is okay
// since we don't need to validate signatures here.

var missingMembershipFixtures = []string{
	// { alg: HS256 }.{}.junk
	"eyJhbGciOiJIUzI1NiJ9.e30.Z",
	// { alg: HS256 }.{ ids_attributes: null }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6bnVsbH0.Z",
	// { alg: HS256 }.{ ids_attributes: {} }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6e319.Z",
	// { alg: HS256 }.{ ids_attributes: { membership: null } }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJtZW1iZXJzaGlwIjpudWxsfX0.Z",
}

func TestMissingMembership(t *testing.T) {
	for k, jwt := range missingMembershipFixtures {
		payload := FromRaw(jwt)
		got := payload.Membership()
		if got != "" {
			t.Errorf("[%v] want: empty; got: %v", k, got)
		}
	}
}

var membershipFixtures = []struct {
	jwt  string
	want string
}{
	// { alg: HS256 }.{ ids_attributes: { membership: true } }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJtZW1iZXJzaGlwIjp0cnVlfX0.",
		"true"},
	// { alg: HS256 }.{ ids_attributes: { membership: "xxx" } }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJtZW1iZXJzaGlwIjoieHh4In19.",
		"xxx"},
}

func TestMembership(t *testing.T) {
	for k, d := range membershipFixtures {
		payload := FromRaw(d.jwt)
		got := payload.Membership()
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}

var missingSecProfileAuditLoggingFixtures = []string{
	// { alg: HS256 }.{}.junk
	"eyJhbGciOiJIUzI1NiJ9.e30.Z",
	// { alg: HS256 }.{ ids_attributes: null }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6bnVsbH0.Z",
	// { alg: HS256 }.{ ids_attributes: {} }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6e319.Z",
	// { alg: HS256 }.{ ids_attributes: { security_profile: null } }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjpudWxsfX0.Z",
	// { alg: HS256 }.{ ids_attributes: { security_profile: {} } }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7fX19.Z",
	// { alg: HS256 }.{ ids_attributes: { security_profile: { audit_logging: null } } }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7ImF1ZGl0X2xvZ2dpbmciOm51bGx9fX0.Z",
}

func TestMissingSecProfileAuditLogging(t *testing.T) {
	for k, jwt := range missingSecProfileAuditLoggingFixtures {
		payload := FromRaw(jwt)
		got := payload.SecProfileAuditLogging()
		if got != "" {
			t.Errorf("[%v] want: empty; got: %v", k, got)
		}
	}
}

var secProfileAuditLoggingFixtures = []struct {
	jwt  string
	want string
}{
	// { alg: HS256 }.{ ids_attributes: { security_profile: { audit_logging: 2 } } }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7ImF1ZGl0X2xvZ2dpbmciOjJ9fX0.Z",
		"2"},
	// { alg: HS256 }.{ ids_attributes: { security_profile: { audit_logging: "xxx" } } }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7ImF1ZGl0X2xvZ2dpbmciOiJ4eHgifX19.Z",
		"xxx"},
}

func TestSecProfileAuditLogging(t *testing.T) {
	for k, d := range secProfileAuditLoggingFixtures {
		payload := FromRaw(d.jwt)
		got := payload.SecProfileAuditLogging()
		if got != d.want {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}

var missingSecProfileFixtures = []string{
	// { alg: HS256 }.{}.junk
	"eyJhbGciOiJIUzI1NiJ9.e30.Z",
	// { alg: HS256 }.{ ids_attributes: null }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6bnVsbH0.Z",
	// { alg: HS256 }.{ ids_attributes: {} }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6e319.Z",
	// { alg: HS256 }.{ ids_attributes: { security_profile: null } }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjpudWxsfX0.Z",
	// { alg: HS256 }.{ ids_attributes: { security_profile: {} } }.junk
	"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7fX19.Z",
}

func TestMissingSecProfile(t *testing.T) {
	for k, jwt := range missingSecProfileFixtures {
		payload := FromRaw(jwt)
		got := payload.SecProfile()
		if len(got) != 0 {
			t.Errorf("[%v] want: empty; got: %v", k, got)
		}
	}
}

var secProfileFixtures = []struct {
	jwt  string
	want map[string]string
}{
	// { alg: HS256 }.{ ids_attributes: { security_profile: { audit_logging: 2 } } }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7ImF1ZGl0X2xvZ2dpbmciOjJ9fX0.Z",
		map[string]string{"audit_logging": "2"}},
	// { alg: HS256 }.{ ids_attributes: { security_profile: { audit_logging: "xxx" } } }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7ImF1ZGl0X2xvZ2dpbmciOiJ4eHgifX19.Z",
		map[string]string{"audit_logging": "xxx"}},
	// { alg: HS256 }.{ ids_attributes: { security_profile: { audit_logging: -1, x: 0 } } }.junk
	{"eyJhbGciOiJIUzI1NiJ9.eyJpZHNfYXR0cmlidXRlcyI6eyJzZWN1cml0eV9wcm9maWxlIjp7ImF1ZGl0X2xvZ2dpbmciOi0xLCJ4IjowfX19.Z",
		map[string]string{"audit_logging": "-1", "x": "0"}},
}

func TestSecProfile(t *testing.T) {
	for k, d := range secProfileFixtures {
		payload := FromRaw(d.jwt)
		got := payload.SecProfile()
		if !reflect.DeepEqual(d.want, got) {
			t.Errorf("[%v] want: %v; got: %v", k, d.want, got)
		}
	}
}
