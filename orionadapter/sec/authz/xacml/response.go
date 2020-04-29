package xacml

import (
	"encoding/xml"
	"strings"
)

type outcome struct {
	Decision string `xml:"Result>Decision"`
}

const canonicalPermitFlag = "permit"

func isPermitFlag(decision string) bool {
	trimmed := strings.TrimSpace(decision)
	lowerCased := strings.ToLower(trimmed)
	return lowerCased == canonicalPermitFlag
}

// IsPermitDecision returns true just in case the given AuthZ response XML
// indicates authorization was granted, i.e. the response contains a "permit"
// decision. You get an error back if the response isn't valid XML or doesn't
// contain a 'Response>Result>Decision' path.
func IsPermitDecision(responseXML []byte) (bool, error) {
	outcomePath := outcome{}
	if err := xml.Unmarshal(responseXML, &outcomePath); err != nil {
		return false, err
	}
	return isPermitFlag(outcomePath.Decision), nil
}
