package jwt

import (
	"regexp"
)

func parseSubjectCommonName(subject string) (value string, success bool) {
	cnRegex := regexp.MustCompile(`^(CN|.*[, ]CN)[ ]*=[ ]*([^, ]+)([, ]|$)`)
	match := cnRegex.FindStringSubmatch(subject)
	if len(match) >= 3 {
		value = match[2]
		success = value != ""
	}
	return value, success
}
