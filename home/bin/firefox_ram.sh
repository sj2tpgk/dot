#!/bin/sh

if [ $# -ne 1 ] && [ $# -ne 2 ]; then echo "Usage: firefox_ram.sh PROFILE_NAME [MIN_INTERVAL]"; exit 1; fi

# Systemd configuration guide
#
# 1. Create an unit file to run syncing:
#    ~/.config/systemd/user/firefox_ram@.service
#    [Unit]
#    Description=Firefox profile and cache on RAM
#
#    [Service]
#    Type=oneshot
#    ExecStart=/bin/sh %h/bin/firefox_ram.sh %i
#
#    [Install]
#    WantedBy=default.target
#
# 2. Create an timer file to periodically run syncing:
#    ~/.config/systemd/user/firefox_ram@.timer
#    [Unit]
#    Description=Timer for Firefox profile and cache on RAM
#
#    [Timer]
#    OnActiveSec=10s
#    OnUnitInactiveSec=20min
#    # need system restart to apply timer?
#
#    [Install]
#    WantedBy=timers.target
#
# 3. Enable timer, giving a profile name as an argument:
#    $ systemctl --user enable --now firefox_ram@xxxxxxxx.default-release.timer

prof=$1
min_interval=${2:-0}

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
    mt=$(stat -c %Y "$tmp_prof")
    ms=$(stat -c %Y "$disk_prof.static")
    if [ $((mt - ms)) -ge "$min_interval" ]; then
        L rsync -a $rsync_flags --delete --bwlimit=500 "$disk_prof"/ "$disk_prof.static"/
    fi
fi
