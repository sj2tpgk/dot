#!/bin/sh

shell=$1
laststatus=${2:-0}
lastkillsig=${3:-0}

k='\033[30m'  r='\033[31m'  g='\033[32m'  y='\033[33m'
b='\033[34m'  m='\033[35m'  c='\033[36m'  w='\033[37m'
bk='\033[40m' br='\033[41m' bg='\033[42m' by='\033[43m'
bb='\033[44m' bm='\033[45m' bc='\033[46m' bw='\033[47m'

bold='\033[1m' none='\033[0m'

# user and host
# uandh="$USER@$(uname -n | sed 's/.*.\(.....\)$/…\1/')" # hostname at most 5 chars
user=$(echo "$USER" | sed 's/^\(..\)...*\(..\)$/\1.\2/') # at most 5 chars
host=$(uname -n     | sed 's/^\(..\)...*\(..\)$/\1.\2/') # at most 5 chars
uandh="$user@$host"
uh1expr="'("$(echo "$uandh" | sed 's/./%d+/g')"0)%%5\\n'"
uh2args=$(echo "$uandh" | sed "s/./ \\\"'&\\\"/g")
uh3cmds="printf $uh1expr $uh2args | bc" # e.g. printf '(%d+%d+0)%%16\n' "'t" "'p" | bc
# printf '%s\n' "$uh3cmds"
uh4hash=$(sh -c "$uh3cmds")
# printf '%s\n' "$uh4hash"
uh5eseq=$(sh -c 'printf %s ${'"$uh4hash"'}' "$k" "$c" "$g" "$y" "$b" "$m" "$r" "$w")
uandh="$uh5eseq$uandh$none"
# printf '%s\n' "$uh5eseq"
# echo -e "$uh5eseq aaaa $none"
# exit


# time and directory
# time=$(date +%H:%M)
dir=$(pwd | sed 's#^'"$HOME"'/*#~/#; s#/$##; s#\(\.*[^/]\)[^/]*/#\1/#g')

# git
if command -v git >/dev/null && git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    git='('
    git="$git"$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    [ -n "$(git status -s)" ] && git="$git"'+'
    git="$git"')'
fi

# last status and signal when given
if [ "$laststatus" -ne 0 ]; then
    status="[$laststatus"
    if [ "$lastkillsig" -ne 0 ]; then
        sig=$(kill -l "$lastkillsig")
        status="$status""|SIG$sig"
    fi
    status="$status""]"
fi

# prompt arrows
case "$shell" in
    # fish)         arrows="$r>$b<$g($y'$r>" ;;
    fish)         arrows="$r>$y>"; [ -z "$RANGER_LEVEL" ] && arrows="$arrows""$b>" || arrows="$arrows""$g>" ;;
    fish_private) arrows="$m>>$b>" ;;
    bash)         arrows="$y>>$r>" ;;
    nu)           arrows="$m>$r>$y>" ;;
    elvish)       arrows="$bm$w>>>$none" ;;
    *)            arrows="$w>>>" ;;
esac

# combine strings
# s="$none"
s="$s""$c$uandh "
# s="$s""$c$time "
[ -n "$git" ] && s="$s""$m$git "
s="$s""$g$dir "
[ -n "$status" ] && s="$s""$m$r$status "
s="$s""$bold$arrows$none "

# write output
echo "$s" | tr -d '\n'
# echo -ne "$s"      # not posix
# printf "$s"        # potentially misinterpret % in $s as format specifier
# printf '%s' "$s"   # no color

