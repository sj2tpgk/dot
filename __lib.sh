#!/bin/sh

overwriteall=0  # overwrite all confliction
backupall=0     # backup all confliction
skipall=0       # backup all confliction

msg(){ echo -e "$1"; }

ask(){
    # Usage: ask "[y]es, [n]o"
    # $ask_return is set to "y" or "n"
    prompt="$1"
    cands=$(echo "$prompt" | sed 's/\[\(.\)\][^\[]*/\1/g')
    while true; do
        read -p "$prompt: " ask_return
        len=$(echo "$ask_return" | wc -m)
        [ $len -eq 2 -a "_$cands" != "_${cands/$ask_return/}" ] && break
    done
}

safelink(){
    # Usage: safelink <fromPath> <toPath>
    # If toPath already exists, choose from overwrite, skip, backup
    # Necessary intermediate directories are automatically created.

    from="$1"
    to="$2"

    exist=0 skip=0 backup=0 overwrite=0
    msg "Linking $1 to $2."
    # if [ ! -f "$from" ]; then msg "Directory not allowed! Skipping."; return; fi
    if [ -e "$to" -o -h "$to" ]; then # -h for broken symlinks
        exist=1
        if [ $skipall != 1 -a $overwriteall != 1 -a $backupall != 1 ]; then
            echo "File/directory exists at $to. Overwrite?"
            ask "[y]es, [Y]es all, [s]kip, [S]kip all, [b]ackup, [B]ackup all"
            case $ask_return in
                y) overwrite=1 ;;
                Y) overwrite=1 overwriteall=1 ;;
                s) skip=1 ;;
                S) skip=1 skipall=1 ;;
                b) backup=1 ;;
                B) backup=1 backupall=1 ;;
            esac
        fi
    fi

    if [ $exist = 0 ]; then dolink=1
    else

        # Warning when removing many files
        if [ -d "$to" -a $(ls -1 "$to" | wc -l) -ge 30 ]; then
            echo Trying to remove many files, you might be doing something wrong.
            echo Do you really continue?
            ask "[y]es, [n]o"
            [ $ask_return = n ] && exit
        fi

        # Backup or overwrite and decide whether to skip.
        if [ $skip = 1 ]; then dolink=0
        else
            [ $backup = 1 -o $backupall = 1 ] && mv "$to" "${to}_$(date +%Y%m%d_%H%M%S)" && echo Backuped "$to"
            [ $overwriteall = 1 -o $overwrite = 1 ] && rm -r "$to" && echo Overwritten "$to"
            [ $skipall = 1 ] && dolink=0 || dolink=1
        fi

    fi

    # Now $to is guaranteed to be non-existent.
    if [ $dolink = 0 ]; then msg "Skipping."
    else
        todir=$(dirname "$to")
        # msg "Directory $todir"
        mkdir -p "$todir"
        ln -s "$from" "$to"
    fi

}

# safelink ~/dot/home ~/dot/homelink
