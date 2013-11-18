#!/bin/sh

FILENAME=$1

usage() {
  cat << EOF
Usage: $0 FILENAME
EOF
  exit 1
}

sed -i \
  -e 's/。/．/g' \
  -e 's/、/，/g' \
  -e 's/(/（/g' \
  -e 's/)/）/g' \
  $FILENAME
