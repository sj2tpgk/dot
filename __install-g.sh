#!/bin/sh


SCRIPTPATH=$(realpath -s "$0")
SCRIPTDIR=$(dirname "$SCRIPTPATH")
DOTHOME=${1:-$SCRIPTDIR/home-g}
REALHOME=${2:-~}

./__install.sh "$DOTHOME" "$REALHOME"
