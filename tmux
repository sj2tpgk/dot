#!/bin/sh
#vim: fdm=marker fdl=0 fen

# https://codeberg.org/sj2tpgk/dot/raw/branch/master/tmux

errexit() { printf "\033[31;1mERROR: \033[0m%s\n" "$1" >&2; exit 1; }

# Env vars and paths
#   TMUX_ROOT          temporary files are stored here
#   TMUX_SHELL         bash or fish; use this shell for default-command
#   TMUX_SOCKET_NAME   socket name (if set externally, use that one)
#   TMUX_bash          bash
#   TMUX_fish          fish
#   TMUX_cleanup       removes temporary files. run when last session dies

# Environment vars
TMUX_ROOT="$PREFIX"/tmp/tmux-tmp/
export TMUX_ROOT="$TMUX_ROOT"

# Get abs path of this script
# TMUX_SCRIPT="$(cd "$(dirname "$0")"; pwd)" || errexit "could not cd to \$(dirname $0)"
# TMUX_SCRIPT="$abs"/"$(basename "$0")"
# export TMUX_SCRIPT="$TMUX_SCRIPT"

# Init (called when this script is invoked)
_init() {

    # Create "$TMUX_ROOT" directory
    [ "${TMUX_ROOT#$PREFIX/tmp/}" = "$TMUX_ROOT" ] && errexit "TMUX_ROOT $TMUX_ROOT is not in /tmp/"
    if [ -d "$TMUX_ROOT" ]; then
        rm -r "$TMUX_ROOT"
        mkdir -p "$TMUX_ROOT"
    else
        mkdir -p "$TMUX_ROOT"
    fi

    # Create $TMUX_ROOT/bin and add it to $PATH
    mkdir -p "$TMUX_ROOT/bin"
    export PATH="$TMUX_ROOT/bin:$PATH"

    # Extract files from this script
    # TODO optimize
    # Awk prints <NR> <permission> <file> e.g. "10 +x bin/foo"
    # Directives:
    #   ####FILE <permission> <file>       Mark the beginning of the file (ends before next ####FILE)
    #                                      (optionally followed by one apace and three open-brace, will be removed)
    #   #| <any>                           Replaced with <string> (may help sh syntax highlighting)
    #   #- <any>                           This line is removed
    awk '/^####FILE/{sub("####FILE ","");sub("{""{{$","");print NR" "$0}' "$0" | while read -r nr permission file; do
        path="$TMUX_ROOT"/"${file##/*}"
        dir=$(dirname "$path")
        # echo "def nr=[$nr], permission=[$permission], path=[$path], dir=[$dir]"
        [ ! -d "$dir" ] && mkdir -p "$dir"
        sed "1,${nr}d;/^####/Q;/^#- /d;s/^#| //" "$0" > "$path"
        case "$permission" in
            ("--") ;;
            ("+x") chmod +x "$path" ;;
            ("*")  errexit "invalid permission specified: $permission (file=$file)" ;;
        esac
    done

}

printf '\033[2 q' # block cursor

if :; then
    # Regen conf
    _init
    # Export env vars
    TMUX_SOCKET_NAME=${TMUX_SOCKET_NAME:-default}
    export TMUX_SOCKET_NAME="$TMUX_SOCKET_NAME"

    if [ 0 -eq $(tmux -L "$TMUX_SOCKET_NAME" list-sessions 2>/dev/null | wc -l) ]; then
        # If no session => create one
        tmux -L "$TMUX_SOCKET_NAME" -f "$TMUX_ROOT/tmux.conf"
    else
        # If session exists => reload config and run tmux command
        echo Regenerated files in "$TMUX_ROOT"
        tmux -L "$TMUX_SOCKET_NAME" -f "$TMUX_ROOT/tmux.conf" source "$TMUX_ROOT/tmux.conf"
        tmux -L "$TMUX_SOCKET_NAME" -f "$TMUX_ROOT/tmux.conf" "$@"
    fi
fi

exit # Do not remove this exit
# ==== Shell script ends here ====


# Now follows embedded files


####FILE +x bin/du1 {{{
#!/bin/sh
du -d1 -BM "$@" 2>/dev/null | sort -n
#- }}}


####FILE +x bin/ra {{{
#!/bin/sh
f() { echo "$@"; "$@"; }
# f() { echo "$@"; }
[ "$MYKBD" = colemakdh ] && flag=a || a=""
# Note: $(x=${flag:+val1}; echo ${x:-val2}) is val1 if $flag is nonempty else val2
exec ranger --clean \
    --cmd "set show_hidden       on" \
    --cmd "set tilde_in_titlebar on" \
    --cmd "set shorten_title     3" \
    --cmd "set update_title      on" \
    --cmd "map S shell $(tmux -L "$TMUX_SOCKET_NAME" show -qvg default-command)" \
    --cmd "map $(x=${flag:+k}; echo ${x:-h}) move left=1" \
    --cmd "map $(x=${flag:+n}; echo ${x:-j}) move down=1" \
    --cmd "map $(x=${flag:+e}; echo ${x:-k}) move up=1" \
    --cmd "map $(x=${flag:+i}; echo ${x:-l}) move right=1" \
    --cmd "map $(x=${flag:+K}; echo ${x:-H}) history_go -1" \
    --cmd "map $(x=${flag:+I}; echo ${x:-L}) history_go +1" \
    --cmd "map $(x=${flag:+N}; echo ${x:-J}) move down=0.5 pages=True" \
    --cmd "map $(x=${flag:+E}; echo ${x:-K}) move up=0.5   pages=True" \
    --cmd "map $(x=${flag:+j}; echo ${x:-n}) search_next" \
    --cmd "map $(x=${flag:+J}; echo ${x:-N}) search_next forward=False" \
    --cmd "map $(x=${flag:+L}; echo ${x:-I}) eval fm.open_console('rename ' + fm.thisfile.relative_path.replace('%', '%%'), position=7)" \
    "$@"
#- }}}



####FILE +x bin/termux-my-config {{{
#!/bin/sh
command -v termux-reload-settings || { echo "termux-reload-settings is not available"; exit 1; }
[ -z "$1" ] && color=light || color=dark
[ -z "$1" ] && echo "Using light mode. Use 'termux-my-config 1' for dark mode"
COLORFILE=~/.termux/colors.properties
PROPFILE=~/.termux/termux.properties
[ -e "$COLORFILE" ] && { cp "$COLORFILE" "$COLORFILE.$(date +%s)"; }
[ -e "$PROPFILE" ] && { cp "$PROPFILE" "$PROPFILE$(date +%s)"; }
if [ "$color" = light ]; then
    {
        echo "foreground=#000"
        echo "background=#fff"
        echo "cursor=#722"
        echo
        echo "color0=#e8e6e4"
        echo "color1=#ef5253"
        echo "color2=#5ca824"
        echo "color3=#c49500"
        echo "color4=#33b5e1"
        echo "color5=#a363d5"
        echo "color6=#32ab90"
        echo "color7=#18262f"
        echo
        echo "color8=#b6bfc8"
        echo "color9=#ff6263"
        echo "color10=#6cb834"
        echo "color11=#d4a50c"
        echo "color12=#23a5d1"
        echo "color13=#b373d5"
        echo "color14=#42bba0"
        echo "color15=#78868f"
    } > "$COLORFILE"
else
    {
        echo "foreground=#D0D0D0"
        echo "background=#151515"
        echo "cursor=#ffcccc"
        echo
        echo "color0=#18262f"
        echo "color1=#ef5253"
        echo "color2=#7cc844"
        echo "color3=#e4b51c"
        echo "color4=#33b5e1"
        echo "color5=#a363d5"
        echo "color6=#52cbb0"
        echo "color7=#a6afb8"
        echo
        echo "color8=#78868f"
        echo "color9=#ff6263"
        echo "color10=#8cd854"
        echo "color11=#f4c52c"
        echo "color12=#43c5f1"
        echo "color13=#b373d5"
        echo "color14=#62dbc0"
        echo "color15=#b6bfc8"
    } > "$COLORFILE"
fi
{
    echo "# vibrate, beep, ignore"
    echo "bell-character=ignore"
    echo
    echo "# back, escape"
    echo "back-key=back"
    echo
    echo "extra-keys = [ \\"
    echo " ['ESC', '~', '/',    '|',   '{', '}', 'HOME', 'UP',   'END'  ], \\"
    echo " ['TAB', '>', 'CTRL', 'ALT', '[', ']', 'LEFT', 'DOWN', 'RIGHT']  \\"
    echo "]"
} > "$PROPFILE"
termux-reload-settings
#- }}}


####FILE +x bin/tmux-comp {{{
#!/bin/sh

info=$(tmux display -pF 'pane_id=#{pane_id}:pane_left=#{pane_left}:pane_top=#{pane_top}:cursor_x=#{cursor_x}:cursor_y=#{cursor_y}:client_height=#{client_height}')
tmp=${info#*pane_id=};       t=${tmp%%:*}
tmp=${info#*pane_left=};     px=${tmp%%:*}
tmp=${info#*pane_top=};      py=${tmp%%:*}
tmp=${info#*cursor_x=};      cx=${tmp%%:*}
tmp=${info#*cursor_y=};      cy=${tmp%%:*}
tmp=${info#*client_height=}; h=${tmp%%:*}

export q
q=$(tmux capturep -J -p -S "$cy" -E "$cy" | cut -c-"$cx" | grep -oE '\w+$' || echo)

p1='[[:alnum:]]{4,}'
p2='[-+@.[:alnum:]]{4,}'
p3='[-+@./[:alnum:]]{4,}'

cs="tmux "
for i in $(tmux lsp -a -F '#D'); do cs="$cs capturep -J -pt $i \; "; done
# cs="$cs | grep -oE '\w{4,}' | awk -v q=\"\$q\" 'substr(\$0,1,length(q))==q{print}' | sort -u"
# cs="$cs | { { { tee /dev/fd/3 | grep -oE '$p1' >&4; } 3>&1 | grep -oE '$p2'; } 4>&1; } | awk -v q=\"\$q\" 'substr(\$0,1,length(q))==q{print}' | sort -u"
cs="$cs | { { { { tee /dev/fd/4 /dev/fd/5 | grep -oE '$p1' >&3; } 4>&1 | grep -oE '$p2' >&3; } 5>&1 | grep -oE '$p3'; } 3>&1; } | awk -v q=\"\$q\" 'substr(\$0,1,length(q))==q{print}' | sort -u"

if ! command -v fzf >/dev/null; then
    s=$(eval "$cs" | head -n $((h - 2)) | awk -v q="$q" -v l="${#q}" "{a=\$0; printf \"'%s' '%c' '\", a, NR+47+(NR>10)*39-(NR>36)*58; if (l) { printf \"send -N %d BSpace ; \", l } printf \"send -l \\\"%s\\\"' \", a}")
    eval tmux menu "$s"
    exit
fi

cmd='
  s=$(eval "$cs" | fzf --no-color --color bw --info hidden --prompt "  " --pointer " " --print-query -q "$q")
  [ $? -ne 130 ] && { s=$(echo "$s" | tail -n1); tmux ${q:+send -t "$t" -N "${#q}" BSpace \;} send -t "$t" -l "$s " 2>/dev/null; }
  exit 0'
tmux popup -EB -e "cs=$cs" -e "t=$t" -e "q=$q" -w 35 -h 8 -x $(expr $px + $cx - "${#q}" - 2) -y $(expr $py + $cy + 1) "$cmd" \
    || tmux splitw -e "cs=$cs" -e "t=$t" -e "q=$q" -l 8 "$cmd"
#- }}}


####FILE +x bin/TMUX_bash {{{
#!/bin/sh
INPUTRC="$TMUX_ROOT/inputrc" exec bash --rcfile "$TMUX_ROOT/bashrc"
#- }}}


####FILE +x bin/TMUX_cleanup {{{
#!/bin/sh
# Remove $TMUX_ROOT dir when all tmux sessions are closed
if [ "$(tmux -L "$TMUX_SOCKET_NAME" list-sessions | wc -l)" = 0 ]; then
    # Ensure not to remove wrong thing
    [ "${TMUX_ROOT#$PREFIX/tmp/}" = "$TMUX_ROOT" ] && exit 1
    [ ! -d "$TMUX_ROOT" ] && exit 1
    rm -r "$TMUX_ROOT"
fi
#- }}}


####FILE +x bin/TMUX_fish {{{
#!/bin/sh
sav=$XDG_CONFIG_HOME
export XDG_CONFIG_HOME="$TMUX_ROOT/config"
export SHELL=TMUX_fish
if [ -z "$sav" ]; then
    exec fish -C "set -e XDG_CONFIG_HOME"
else
    exec fish -C "set -xg XDG_CONFIG_HOME '$sav'"
fi
#- }}}


####FILE +x bin/v {{{
#!/bin/sh
if command -v vim >/dev/null; then
    SHELL=sh vim -u "$TMUX_ROOT/vimrc" "$@"
elif command -v nvim >/dev/null; then
    SHELL=sh nvim -u "$TMUX_ROOT/vimrc" "$@"
elif command -v nano >/dev/null; then
    nano "$@"
elif command -v emacs >/dev/null; then
    emacs -nw -Q --color=yes "$@"
elif command -v vi >/dev/null; then
    vi "$@"
else
    echo "No editor found"
fi
#- }}}


####FILE -- config/fish/config.fish {{{
[ -d "/data/data/com.termux/files/home/bin/" ] \
    && set -x PATH $PATH:/data/data/com.termux/files/home/bin/
set fish_color_autosuggestion 'magenta'
#| function fish_greeting; end
#| function mkcd; mkdir $argv[1] && cd $argv[1]; end
#| for i in f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12; bind -k $i ""; end
#| for i in \e\[25\;2~ \e\[26\;5~; bind $i ""; end
#| bind \eg "commandline -r (commandline -b | sed 's#\s*\$# | grep -i #')"
#| command -v doas >/dev/null 2>&1 && alias sudo doas
#- }}}


####FILE -- bashrc {{{
conf() { sed "0,/^#===$1/d;/^#===/Q;s/^###//" "$TMUX_CONF"; }
command -v nvim >/dev/null && nv() { nvim -u <(conf vim) "$@"; }
command -v vim  >/dev/null && v()  { vim  -u <(conf vim) "$@"; }
_exitstatus() { local s=$?; [[ $s == 0 ]] && echo "" || echo -e "\e[31m$s\e[0m "; }
export PS1="\[\e[36m\]\w\[$(tput sgr0)\] \$(_exitstatus)>>> "
mkcd() { mkdir "$1" && cd "$1"; }
alias la='ls -la'
#- }}}


####FILE -- inputrc {{{
Tab:    menu-complete
"\e[Z": complete
"\C-w": shell-backward-kill-word
"\e[A": history-search-backward
"\e[B": history-search-forward
set show-all-if-ambiguous        on  # Tab -> partial completion and show candidates
set show-all-if-unmodified       on  #
set colored-stats                on  # Color files by types
set visible-stats                on  # Append char to indicate type
set mark-symlinked-directories   on  # Mark symlinked directories
set colored-completion-prefix    on  # Color the common prefix
set menu-complete-display-prefix on  # Color the common prefix in menu-complete
set eo-control-characters        off # Don't show ^C etc.
set enable-bracketed-paste       off # Workaround (https://github.com/hanslub42/rlwrap/issues/108)
set completion-ignore-case       on  # Case insensitive completion
#- }}}


####FILE -- tmux.conf {{{
set -g default-shell /bin/sh
set-hook -g session-closed 'run TMUX_cleanup'

# Seems "setenv -g" does not affect variables within .tmux.conf (but do affect in child proc)
EDITOR="$TMUX_ROOT/bin/v"
VISUAL="$TMUX_ROOT/bin/v"

# Set shell ( if $TMUX_SHELL is set, use that shell )
if "if [ -n '#{TMUX_SHELL}' ]; then [ fish = '#{TMUX_SHELL}' ]; else command -v fish >/dev/null; fi" {
    set -g default-command 'TMUX_fish'
    # set -g default-command 'exec fish -C "function conf; sed \"0,/^#===\$argv[1]/d;/^#===/Q;s/^###//\" \"'"$TMUX_CONF"'\"; end; conf fish | source"' # exec needed? to change cwd of pane process
} {
    set -g default-command 'TMUX_bash'
    # set -g default-command 'd=$(mktemp -d "$PREFIX"/tmp/tmux-temp-conf-XXXXXX); sed "0,/^#===inputrc/d;/^#===/Q;s/^###//" '"$TMUX_CONF"' > "$d/1"; sed "0,/^#===bash/d;/^#===/Q;s/^###//" '"$TMUX_CONF"' > "$d/2"; INPUTRC="$d/1" bash --init-file "$d/2"; rm "$d/1" "$d/2"; rmdir "$d"'
}

set -g escape-time 0
set -g mouse       on

set -g status-position    top
set -g status-left        '[#S] '
set -g status-left-length 4
set -g status-right       '#{s#^'$HOME'#~#;s#/\$##;s#([-._]*[^/])[^/]*/#\1/#g:pane_current_path} [#{s/^(..)...*(..)$/\1.\2/:user}@#{s/^(..)...*(..)$/\1.\2/:host}]'
set -g pane-active-border-style fg=yellow

bind    '"' split-window -vc "#{pane_current_path}"
bind    s   split-window -vc "#{pane_current_path}"
bind    %   split-window -hc "#{pane_current_path}"
bind    v   split-window -hc "#{pane_current_path}"
bind    w   new-window   -c  "#{pane_current_path}"
bind    r   run "$TMUX_SCRIPT" \; source "$TMUX_ROOT/tmux.conf" \; display "Reloaded .tmux.conf"
bind -r o   select-pane -t :.+
bind -r n   next-window
bind -n F3  next-window
bind -n F4  select-pane -t :.+

# Completion using fzf
bind    Tab      run "$TMUX_ROOT/bin/tmux-comp"

if-shell "test -f '$HOME/.tmux.conf.local'" { source "$HOME/.tmux.conf.local" }
#- }}}


####FILE -- vimrc {{{
#| " vim
#| if filereadable($VIMRUNTIME . "/defaults.vim") | source $VIMRUNTIME/defaults.vim | endif
#| aug view|exe "au!"|exe "au BufWinLeave * mkvie"|exe "au BufWinEnter * sil! lo"|aug END
#| set et nocp sm hid
#| syntax on| filetype on| filetype plugin indent on
#| set ls=2 stl=[%{&readonly?'R':''}%{&modified?'+':'-'}]\ \ %<%f%*%=%-10.(%l,%c%V%)\ %y%6.(%P%)
#| " neovim
#| set ai sw=4 ts=4 sr mouse=av so=1 siso=5 tw=0 nu sb spr cf acd wic ic scs is hls ttm=0 t_Co=16
#| for i in range(1, 16) | for j in ["", "s-", "c-"] | for k in ["", "i", "c"] | exe k . "nore <" . j . "f" . i . "> <nop>" | endfor | endfor | endfor
#| nn Q :q<cr>| ino <s-f13> <nop>| ino <c-f14> <nop>| nn D dd| nn Y yy| nn ss :sp<cr>| nn sv :vsp<cr>| nn sb :bd<cr>| nn so <c-w>o| nn :<cr> :wa<cr>| nn <a-j> J| nn - <c-w>w| nnore + :tabnext<cr>| nn > >>| nn <lt> <lt><lt>| nn U <c-r>
#| " no <esc>[25;2~ <nop>| ino <esc>[25;2~ <nop>| no <esc>[26;5~ <nop>| ino <esc>[26;5~ <nop>
#| no j gj| no k gk| no gj j| no gk k| no J <c-d>| no K <c-u>| no h h| no l l| no gh 0| no gl <end>| no i i| no I I| no si s| no n n| no N N| no e e| no E E| ono e e| ono E E| ono h 0| ono l $| ono iw iw| ono iW iW
#| if $MYKBD == "colemakdh" | no n gj| no e gk| no gn j| no ge k| no N <c-d>| no E <c-u>| no k h| no i l| no gk 0| no gi <end>| no l i| no L I| no sl s| no j n| no J N| no h e| no H E| ono h e| ono H E| ono k 0| ono i $| ono lw iw| ono lW iW| endif
#| ono m %|nn m %
#| exe "au InsertEnter * set cul"| exe "au InsertLeave * set nocul"

#| " Basic auto completion
#| inore <tab>       <c-n>
#| inore <plug>MyTab <c-n>
#| inore <s-tab>     <c-p>
#| inore <expr> <cr> pumvisible() ? "\<c-y>\<cr>" : "\<cr>"
#| set shm+=c cot=menuone,noinsert,noselect inf
#| " Auto complete (https://stackoverflow.com/questions/35837990)
#| fu! OpenCompletion()
#|     " check (menu invisible && inserting iskeyword char && at least minlen chars)
#|     let minlen = 2
#|     if !pumvisible() && (v:char =~ '\K') && (minlen == 1 || (col(".") >= (minlen-1) && matchstr(getline("."), '\%' . (col('.')-(minlen-1)) . 'c\K\{' . (minlen-1) . '\}') != ""))
#|         call feedkeys("\<plug>MyTab", "")
#|         " feedkeys("\<c-n>", "") will mess up repeating
#|     endif
#| endfu
#| " au InsertCharPre * call OpenCompletion()
#| hi Pmenu ctermbg=black ctermfg=magenta
#- }}}
