#!/bin/sh

# By default symlink file to file. Place ".dot.whole" to link a whole directory.

source ./__lib.sh

SCRIPTPATH=$(realpath -s "$0")
SCRIPTDIR=$(dirname "$SCRIPTPATH")
DOTHOME=${1:-$SCRIPTDIR/home}
REALHOME=${2:-~/}

rec() {
    # Example: rec .
    # Works similar to "find . -type f" except if a directory D contains a file
    # named ".dot.whole", D is appended to the result instead of its decendants.
    # Results are stored in $rec_result, each splitted by a '\n'.

    # Depth-first search using a stack.

    sep=$'\n'
    stack="$1$sep"
    result=""
    OLDGLOBIGNORE=$GLOBIGNORE GLOBIGNORE=".:.."
    OLDIFS=$IFS IFS=$'\n'

    while [ "$stack" != "" ]; do
        # echo "STACK=$stack"; echo "RESUL=$result"

        # pop
        cur="${stack%%$sep*}"
        stack="${stack#*$sep}"
        # echo "CUR=$cur"

        # accumulate results
        if [ -e "$cur/.dot.whole" ]; then
            result="$cur$sep$result"
        else
            for f in "$cur/"*; do
                # Don't add "-h" here; it will treat symlink to .emacs.d as a
                # file fox example.
                if [ -f "$f" ]; then result="$f$sep$result";
                elif [ -d "$f" ]; then stack="$f$sep$stack"; fi
            done
        fi

    done

    IFS=$OLDIFS
    GLOBIGNORE=$OLDGLOBIGNORE
    # echo "STACK=$stack"; echo "RESUL=$result"

    rec_return=$result
}

main() {
    msg "-- begin --"

    echo "Trying to install from $DOTHOME to $REALHOME, ok?"
    ask "[y]es, [n]o"
    # ask_return=y
    if [ $ask_return = n ]; then
        echo "Usage:   ./__install.sh [from] [to]"
        echo "Example: ./__install.sh ./home \$HOME"
    else

        DOTHOME=$(readlink -f "$DOTHOME")
        REALHOME=$(readlink -f "$REALHOME")

        if [ ! -d "$DOTHOME" ]; then
            echo "Directory $DOTHOME doesn't exist!"
            exit
        fi

        cd "$DOTHOME"

        rec .
        paths=$rec_return

        # "find . | while read ..." not work here, since safelink read from stdin
        OLDIFS=$IFS
        IFS=$'\n'
        for f in $paths; do
            safelink "$DOTHOME/$f" "$REALHOME/$f"
        done
        IFS=$OLDIFS

    fi

    msg "-- finished --"
}

main

