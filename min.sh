#!/bin/sh

prefix='https://github.com/sj2tpgk/dot/raw/master/'

if command -v wget >/dev/null; then
    downloader=wget
elif command -v curl >/dev/null; then
    downloader=curl
else
    echo "no downloader found" >&2
    exit 1
fi

f() {
    permission=$1
    ifile=$2
    ofile=$3
    outdir=$(dirname "$ofile")
    [ ! -d  "$outdir" ] && { mkdir -p "$outdir" || { echo "failed: mkdir -p $outdir" >&2; exit 1; }; }
    case "$downloader" in
        wget) wget "$prefix/$ifile" -O "$ofile" ;;
        curl) curl "$prefix/$ifile" >  "$ofile" ;;
    esac
    [ "$permission" = x ] && chmod +x "$ofile"
}

f - home/.config/nvim/init.vim    ~/.config/nvim/init.vim
f - home/.config/fish/config.fish ~/.config/fish/config.fish
f - home/.config/ranger/rc.conf   ~/.config/ranger/rc.conf
f x home/bin/myprompt             ~/bin/myprompt
f - home/.tmux.conf               ~/.tmux.conf
