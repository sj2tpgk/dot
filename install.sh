#!/bin/sh

die() { printf "\033[1;31m%s\033[0m\n" "$*"; exit 1; }

help() {
    echo "Usage: $0 [-o] SRCDIR DESTDIR"
    echo "Options:"
    echo "    -o  override"
}


## Parse params

oflag=0
VARS="SRC DEST " # positional arguments
while [ $# -ge 1 ]; do
    case "$1" in
        (-o) oflag=1; shift ;;
        ("") help; die "got empty string as argument" ;;
        (*) if [ "$VARS" ]; then eval "${VARS%% *}=\$1"; VARS=${VARS#* }; shift; else help; die "got extra argument"; fi; ;;
    esac
done
[ "$SRC" ] && [ "$DEST" ] || { help; die "not enough arguments"; }
echo "from [$SRC]"
echo "to   [$DEST]"
echo "override = $oflag"


## Check params

[ -d "$SRC" ] || die "not a directory: $SRC"
[ -d "$DEST" ] || die "not a directory: $DEST"

SRC_ABS=$(cd "$SRC" && pwd) || die "cannot cd to $SRC"
DEST_ABS=$(cd "$DEST" && pwd) || die "cannot cd to $DEST"


## Helpers

d() { echo "$*"; "$@"; }

link() {
    [ "$#" -eq 2 ] || die "Usage: link SRC DEST"
    set -- "$1" "$2" "$(dirname "$2")"
    [ -d "$3" ] || mkdir -p "$3"
    [ -d "$3" ] || die "Could not create dest directory: $3"
    [ 0 = "$oflag" ] && [ -e "$2" ] && { die "file/dir already exists (use -o to override): $2"; }
    d ln -snf "$1" "$2"
}

rec() {
    # $1 and $2 must be absolute path
    if [ -e "$1/.dot.whole" ]; then
        link "$1" "$2"
    elif [ -f "$1" ]; then
        link "$1" "$2"
    elif [ -d "$1" ]; then
        cd "$1" || die "cannot cd to $1"
        for i in ./* ./.*; do
            # note: $i is global but safe
            # note: cwd may change after rec
            if [ -e "$1/$i" ] || [ -L "$1/$i" ]; then
                rec "$1/$i" "$2/$i"
            fi
        done
    else
        die "not a regular file or a directory: $1"
    fi
}


## Main

rec "$SRC_ABS" "$DEST_ABS"
