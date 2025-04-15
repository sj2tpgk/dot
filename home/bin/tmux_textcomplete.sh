#!/usr/bin/env bash

Regexes='[[:alnum:]]{4,}   [-+@._[:alnum:]]{4,}   [-+@._/:[:alnum:]]{4,}'
FzfOpts="--no-color --color bw --info hidden"
Scrollback=-5
PopupWidth=35
PopupHeight=8

if [[ $# -eq 0 ]]; then

    # Get info and options (combine to single tmux call for performance)
    info=$(tmux display-message -pF 'client_height=#{client_height}:version=#{version}:pane_id=#{pane_id}:pane_left=#{pane_left}:pane_top=#{pane_top}:cursor_x=#{cursor_x}:cursor_y=#{cursor_y}')
    tmp=${info#*version=};       version=${tmp%%:*}; version=${version//[^0-9]/}
    tmp=${info#*client_height=}; winheight=${tmp%%:*}
    tmp=${info#*pane_id=};       curpane=${tmp%%:*}
    tmp=${info#*pane_left=};     px=${tmp%%:*}
    tmp=${info#*pane_top=};      py=${tmp%%:*}
    tmp=${info#*cursor_x=};      cx=${tmp%%:*}
    tmp=${info#*cursor_y=};      cy=${tmp%%:*}

    query=$(tmux capture-pane -J -p -S "$cy" -E "$cy")
    query=$(echo "${query:0:$cx}" | grep -oE '\w+$' || echo)
    # do not use cut for substring, because it counts wide characters etc incorrectly

    if [[ $version -ge 33 ]] && command -v fzf >/dev/null; then
        # popup and fzf available
        tmux display-popup -EB \
            -w "$PopupWidth" -h "$PopupHeight" \
            -x $((px + cx - "${#query}" - 2)) -y $((py + cy + 2)) \
            -e curpane="$curpane" -e winheight="$winheight" -e query="$query" \
            -d "$PWD" "$0 popup"
    else
        # fallback to menu
        curpane="$curpane" winheight="$winheight" query="$query" px="$px" py="$py" cx="$cx" cy="$cy" \
            "$0" menu
    fi

else

    # Capture contents of all panes
    capture_cmd=(tmux)
    for pane_id in $(tmux list-panes -a -F '#{pane_id}'); do
        capture_cmd+=(capture-pane -J -p -S "$Scrollback" -t "$pane_id" ";")
    done
    capture=$("${capture_cmd[@]}")

    # Extract words (filter by regex, uniq, then prefix matching)
    words=$(for regex in $Regexes; do echo "$capture" | grep -oE "$regex"; done | sort -u | awk -v query="$query" '(substr($0, 1, length(query)) == query) { print }')

    if [[ $1 = popup ]]; then
        # Show completion popup with tmux display-popup and fzf
        sel=$(echo "$words" | fzf $FzfOpts --prompt "  " --pointer " " --print-query -q "$query")
        sel_code=$?

        if [[ "$sel_code" -ne 130 ]]; then
            # User selected something
            sel=$(echo "$sel" | tail -n1)
            input_cmd=(tmux)
            [[ "$query" ]] && input_cmd+=(send -t "$curpane" -N "${#query}" BSpace ";")
            input_cmd+=(send -t "$curpane" -l "$sel ")
            # Simulate key input
            "${input_cmd[@]}" 2>/dev/null
        fi

    elif [[ $1 = menu ]]; then
        # Show completion popup with tmux display-menu

        # Construct menu command
        menu_cmd=(tmux display-menu -M -T "Completion" -x $((px + cx - "${#query}" - 2)) -y $((py + cy + 1)))
        OLDIFS=$IFS; IFS=$'\n'; i=0; imax=$((winheight - 4))
        keys=0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ
        for word in $words; do
            action=""
            [[ "$query" ]] && action="send -t '$curpane' -N '${#query}' BSpace ; "
            word_esc=${word//"'"/"'\"'\"'"} # escape single quote
            action="$action send -t '$curpane' -l '$word_esc '"
            menu_cmd+=("$word" "${keys:$i:1}" "$action") # name, key, tmux command to execute (simulate key input)

            i=$((i + 1)); [[ "$i" -ge "$imax" ]] && break
        done
        IFS=$OLDIFS

        # Show menu
        "${menu_cmd[@]}"

    fi

fi

exit
