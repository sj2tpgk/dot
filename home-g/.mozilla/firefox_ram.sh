#!/bin/sh

if [ $# -ne 1 ] && [ $# -ne 2 ]; then echo "Usage: firefox_ram.sh PROFILE_NAME [MIN_INTERVAL]"; exit 1; fi

# Run this script to:
# (1) copy firefox profile to RAM (on boot or user login)
# (2) sync profile from RAM to disk (on logout or shutdown, or periodically)
#
# See ~/.config/systemd/user/firefox_ram@.{service,timer}
#
# To enable syncing,
#   systemctl --user enable --now firefox_ram@PROFILE_NAME.service
# Optionally, to enable periodic syncing,
#   systemctl --user enable --now firefox_ram@PROFILE_NAME.timer
# where PROFILE_NAME is the name of your firefox profile (i.e. xxxxxxxx.default-release)

prof=$1
min_interval=${2:-0}

tmp_cache=/tmp/firefox/cache/$prof
tmp_prof=/tmp/firefox/profile/$prof
disk_cache=~/.cache/mozilla/firefox/$prof
disk_prof=~/.mozilla/firefox/$prof

rsync_flags="--info=stats1 --human-readable"

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
    mt=$(stat -c %Y "$tmp_prof")
    ms=$(stat -c %Y "$disk_prof.static")
    if [ $((mt - ms)) -ge "$min_interval" ]; then
        # L rsync -a $rsync_flags --delete --bwlimit=500 "$disk_prof"/ "$disk_prof.static"/
        L rsync -a $rsync_flags --delete "$disk_prof"/ "$disk_prof.static"/
    fi
fi
