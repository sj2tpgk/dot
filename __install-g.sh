#!/bin/sh

# Usage: ./__install-g.sh xxx [REALHOME]

SCRIPTPATH=$(realpath -s "$0")
SCRIPTDIR=$(dirname "$SCRIPTPATH")
DOTHOME=$SCRIPTDIR/home/
REALHOME=${2:-~}
./__install.sh "$DOTHOME" "$REALHOME"

DOTHOME=$SCRIPTDIR/home-g/
./__install.sh "$DOTHOME" "$REALHOME"
