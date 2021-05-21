# Get time in milli seconds
function jump_gettime; echo (string sub -e -6 (date +%s%N)); end

# If called within cooldown, run repeat command
set jump_cooldown 1600
# The time smart_jump was last used.
set jump_lasttime (math (jump_gettime) - $jump_cooldown)
# Last jump was forward or backward?
set jump_cmd none

function smart_jump -d 'Jump like vim-snipe.'
    set -l now (jump_gettime)
    set -l cmd $argv[1]
    if [ (math $now - $jump_lasttime) -le $jump_cooldown ]
        if [ $jump_cmd = $cmd ]
            # \cf \cf => non-reverse
            # \ef \ef => non-reverse
            commandline -f repeat-jump
        else
            # \cf \ef => reverse
            # \ef \cf => reverse
            commandline -f repeat-jump-reverse
        end
    else
        commandline -f $cmd # forward-jump or backward-jump
        set jump_cmd $cmd
    end
    set jump_lasttime $now
end

bind \cf "smart_jump backward-jump"
bind \ef "smart_jump forward-jump"
