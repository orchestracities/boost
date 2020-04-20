#!/usr/bin/env bash

set -e


if [[ -z "${HEADER_VALUE// }" ]];
then
    echo ">>> HEADER_VALUE env var not set properly"
    echo ">>> set HEADER_VALUE as explained in README"
    exit 1
fi


SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOTDIR="$(dirname "$SCRIPTPATH")"
SCRIPTSDIR="${ROOTDIR}/scripts"

source "${SCRIPTSDIR}/cluster-url.sh"


curl -v "${ORION_BASE_URL}/v2/subscriptions" \
     -H "header: ${HEADER_VALUE}" -H 'Content-Type: application/json' \
     -d @- <<EOF
{
  "description": "A subscription to get info about Room1",
  "subject": {
    "entities": [
      {
        "id": "Room1",
        "type": "Room"
      }
    ],
    "condition": {
      "attrs": [
        "pressure"
      ]
    }
  },
  "notification": {
    "http": {
      "url": "http://httpbin.org/post"
    },
    "attrs": [
      "temperature"
    ]
  },
  "expires": "2040-01-01T14:00:00.00Z",
  "throttling": 5
}
EOF
