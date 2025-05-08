#!/bin/sh

if [ $# -ne 1 ]; then echo "Usage: firefox_ram.sh <profilename>"; exit 1; fi

prof=$1

tmp_cache=/tmp/firefox/cache/$prof
tmp_prof=/tmp/firefox/profile/$prof
disk_cache=~/.cache/mozilla/firefox/$prof
disk_prof=~/.mozilla/firefox/$prof

rsync_flags="--info=stats2 --human-readable"

L(){ echo "[$(date +%Y%m%d-%H%M%S)] $*"; "$@"; }

if [ ! -d "$disk_cache.static" ]; then L mv "$disk_cache" "$disk_cache.static"; fi
if [ ! -d "$disk_prof.static"  ]; then L mv "$disk_prof"  "$disk_prof.static";  fi

if [ ! -d "$tmp_cache" ]; then
    L mkdir -p "$tmp_cache"
    L ln -sf "$tmp_cache" "$disk_cache"
    # L rsync -a $rsync_flags "$disk_cache.static"/ "$disk_cache"/
else
    :
    # L rsync -a $rsync_flags --delete --bwlimit=500 "$disk_cache"/ "$disk_cache.static"/
fi

if [ ! -d "$tmp_prof" ]; then
    L mkdir -p "$tmp_prof"
    L ln -sf "$tmp_prof" "$disk_prof"
    L rsync -a $rsync_flags "$disk_prof.static"/ "$disk_prof"/
else
    L rsync -a $rsync_flags --delete --bwlimit=500 "$disk_prof"/ "$disk_prof.static"/
fi
