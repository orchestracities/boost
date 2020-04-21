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


curl -v "${ORION_BASE_URL}/v2/entities/Room1/attrs" \
     -H "header: ${HEADER_VALUE}" -H 'Content-Type: application/json' \
     -X PATCH -d @- <<EOF
{
  "temperature": {
    "value": 21.5,
    "type": "Float"
  },
  "pressure": {
    "value": 703,
    "type": "Float"
  }
}
EOF
