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


curl -v "${ORION_BASE_URL}/v2/entities" \
     -H "header: ${HEADER_VALUE}" -H 'Content-Type: application/json' \
     -d @- <<EOF
{
  "id": "Room1",
  "type": "Room",
  "temperature": {
    "value": 23,
    "type": "Float"
  },
  "pressure": {
    "value": 720,
    "type": "Integer"
  }
}
EOF
