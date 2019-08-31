#!/usr/bin/env bash

set -eu
set -o pipefail

# liberated from https://stackoverflow.com/a/18443300/433393
realpath() {
  OURPWD=$PWD
  cd "$(dirname "$1")"
  LINK=$(readlink "$(basename "$1")")
  while [ "$LINK" ]; do
    cd "$(dirname "$LINK")"
    LINK=$(readlink "$(basename "$1")")
  done
  REALPATH="$PWD/$(basename "$1")"
  cd "$OURPWD"
  echo "$REALPATH"
}

TOOL_PATH=$(realpath .fake)
PAKET_PATH=$(realpath .paket)
FAKE="$TOOL_PATH"/fake
PAKET="$PAKET_PATH"/paket

if ! [ -e "$FAKE" ]
then
  dotnet tool install fake-cli --tool-path "$TOOL_PATH"
fi

if ! [-e "$PAKET"]
then
  dotnet tool install paket --tool-path "$PAKET_PATH"
fi

"$PAKET install"
