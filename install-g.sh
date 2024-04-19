#!/bin/sh

cd "$(dirname "$0")" || exit 1

./lib.sh home   ~ "$@"
./lib.sh home-g ~ "$@"
