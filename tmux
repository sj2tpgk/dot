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
        rm -r -- "$TMUX_ROOT"/*
    else
        mkdir -p "$TMUX_ROOT"
    fi

    # Create $TMUX_ROOT/bin and add it to $PATH
    export PATH="$TMUX_ROOT/bin:$PATH"

    # Extract files from this script
    # TODO optimize quadratic -> linear
    # Awk prints <NR> <permission> <file> e.g. "10 +x bin/foo"
    # Directives:
    #   ####FILE <permission> <file>       Mark the beginning of the file (ends before next ####FILE)
    #                                      (optionally followed by one apace and three open-brace, will be removed)
    #   #| <string>                        Replaced with <string> (may help sh syntax highlighting)
    #   #- <string>                        This line is removed
    if [ 1 -eq 0 ]; then
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
    else
        # improved version: no nested loop, no duplicate mkdir
        awk -v T="$TMUX_ROOT/" -v Q="'" '
            BEGIN{nxs=0}
            /^####/{close(out);out=0}
            END{close(out);cmd="chmod +x";for(i=0;i<nxs;i++){cmd=cmd" "Q xs[i] Q};system(cmd)}
            /^#-/{next}
            out{sub("#\\| ?","");print $0>out}
            /^####FILE/{
              out=T"/"$3;outd=out;sub("/[^/]*$","",outd);
              if(!outds[outd]){system("mkdir -p "Q outd Q);outds[outd]=1}
              if($2=="+x"){xs[nxs++]=out}
            }
        ' "$0"
    fi

}

printf '\033[2 q' # block cursor

if [ "$1" = pack ]; then

    dest="$(dirname "$0")"/tmux2
    { cat "$0"; printf "\n\n\n\n"; } > "$dest"
    for i in ~/bin/*; do
        [ -f "$i" ] || continue
        echo "####FILE +x $(echo "$i" | sed 's#.*/\(bin/[^/]*\)$#\1#') {{""{"
        cat "$i" | sed 's/^/#| /' # space after bar is needed
        printf "#- }}""}\n\n\n"
    done >> "$dest"

else

    # Regen conf
    _init
    # Export env vars
    TMUX_SOCKET_NAME=${TMUX_SOCKET_NAME:-default}
    export TMUX_SOCKET_NAME="$TMUX_SOCKET_NAME"

    if [ -z "$(tmux -L "$TMUX_SOCKET_NAME" list-sessions 2>/dev/null)" ]; then
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


# ==== Now follows embedded files ====


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
    SHELL=sh exec vim -u "$TMUX_ROOT/vimrc" "$@"
elif command -v nvim >/dev/null; then
    SHELL=sh exec nvim -u "$TMUX_ROOT/vimrc" "$@"
elif command -v nano >/dev/null; then
    exec nano "$@"
elif command -v emacs >/dev/null; then
    exec emacs -nw -Q --color=yes "$@"
elif command -v vi >/dev/null; then
    exec vi "$@"
else
    echo "No editor found"
fi
#- }}}


####FILE -- config/fish/config.fish {{{
[ -d "/data/data/com.termux/files/home/bin/" ] \
    && set -x PATH $PATH:/data/data/com.termux/files/home/bin/
set fish_color_autosuggestion magenta
set fish_color_command cyan
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

bind    '"'   split-window -vc "#{pane_current_path}"
bind    s     split-window -vc "#{pane_current_path}"
bind    %     split-window -hc "#{pane_current_path}"
bind    v     split-window -hc "#{pane_current_path}"
bind    w     new-window   -c  "#{pane_current_path}"
bind    r     run "$TMUX_SCRIPT" \; source "$TMUX_ROOT/tmux.conf" \; display "Reloaded .tmux.conf"
bind -r o     select-pane -t :.+
bind -r n     next-window
bind -n F3    next-window
bind -n F4    select-pane -t :.+
bind -r Left  resize-pane -L 1
bind -r Down  resize-pane -D 1
bind -r Up    resize-pane -U 1
bind -r Right resize-pane -R 1
bind -r <     swap-pane -d -t -1
bind -r >     swap-pane -d -t +1

# Completion using fzf
bind    Tab      run "$TMUX_ROOT/bin/tmux-comp"

# Use xcopy if exists (todo: add to main config)
if-shell "command -v xcopy >/dev/null" "set copy-command xcopy"

# hide status on zoom
set-hook -g after-resize-pane 'if "[ #{window_zoomed_flag} -eq 1 ]" "set -g status off" "set -g status on"'

# Load local config
if-shell "test -f '$HOME/.tmux.conf.local'" { source "$HOME/.tmux.conf.local" }
#- }}}


####FILE -- vimrc {{{
#| " vim
#| if filereadable($VIMRUNTIME . "/defaults.vim") | source $VIMRUNTIME/defaults.vim | endif
#| set et nocp sm hid nosc
#| syntax on| filetype on| filetype plugin indent on
#| set ls=2 stl=[%{&readonly?'R':''}%{&modified?'+':'-'}]\ \ %<%f%*%=%-10.(%l,%c%V%)\ %y%6.(%P%)
#| nn s/ :noh<cr>:let @/ = ""<cr>
#| " neovim
#| set ai sw=4 ts=4 sr mouse=av so=1 siso=5 tw=0 nu sb spr cf acd wic ic scs is hls ttm=0 t_Co=16
#| for i in range(1, 16) | for j in ["", "s-", "c-"] | for k in ["", "i", "c"] | exe k . "nore <" . j . "f" . i . "> <nop>" | endfor | endfor | endfor
#| nn Q :q<cr>| ino <s-f13> <nop>| ino <c-f14> <nop>| nn D dd| nn Y yy| nn ss :sp<cr>| nn sv :vsp<cr>| nn sb :bd<cr>| nn so <c-w>o| nn :<cr> :wa<cr>| nn <a-j> J| nn - <c-w>w| nnore + :tabnext<cr>| nn > >>| nn <lt> <lt><lt>| nn U <c-r>| nno <space> <c-d>| nno <backspace> <c-u>| no <cr> K
#| " no <esc>[25;2~ <nop>| ino <esc>[25;2~ <nop>| no <esc>[26;5~ <nop>| ino <esc>[26;5~ <nop>
#| no j gj| no k gk| no gj j| no gk k| no J <c-d>| no K <c-u>| no h h| no l l| no gh 0| no gl <end>| no i i| no I I| no si s| no n n| no N N| no e e| no E E| ono e e| ono E E| ono h 0| ono l $| ono iw iw| ono iW iW| nno H <c-o> |nno L <c-i>
#| if $MYKBD == "colemakdh" | no n gj| no e gk| no gn j| no ge k| no N <c-d>| no E <c-u>| no k h| no i l| no gk 0| no gi <end>| no l i| no L I| no sl s| no j n| no J N| no h e| no H E| ono h e| ono H E| ono k 0| ono i $| ono lw iw| ono lW iW| nno K <c-o> |nno I <c-i> | endif
#| ono m %| nn m %| nn yb mzggVGy'z| nn db mzggVGd'z
#| exe "au InsertEnter * set cul"| exe "au InsertLeave * set nocul"
#|
#| if has("nvim")|set notgc|sil! colo vim|endif
#|
#| " Save/restore view
#| aug view
#| au!
#| au BufWinLeave * if expand('%') != '' && &bt !~ 'nofile' | sil! cal mkdir(&vdir,'p') | mkvie | end
#| au BufWinEnter * if expand('%') != '' && &bt !~ 'nofile' | sil! lo | end
#| aug END
#|
#| " Clipboard
#| set clipboard=unnamedplus
#| " If my clipboard wrapper is available, use it.
#| if executable("xcopy") && executable("xpaste")
#|     let g:clipboard = { 'name': 'my', 'copy': { '+': 'xcopy', '*': 'xcopy', }, 'paste': { '+': 'xpaste', '*': 'xpaste', }, 'cache_enabled': 1, }
#| endif
#|
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
#|
#| " Basic linter integration (press | key)
#| nnore <bar> :sil call Linter(0)<cr>
#| " au CursorHold *.pl call Linter(1)
#| set updatetime=700
#| au FileType sh setl makeprg=shellcheck\ -f\ gcc\ %:S
#| au FileType perl setl makeprg=perlcritic\ --verbose\ 1\ -3\ %:S
#| set ar aw
#| fu! Linter(nomove)
#|     let qf = 0 " quickfix open?
#|     for i in range(1, winnr('$')) | let qf += getwinvar(i, '&syntax') == 'qf' | endfo
#|     if empty(bufname("."))
#|         return
#|     elseif (getbufinfo(".")[0].changed || !qf) && executable(split(&makeprg)[0])
#|         let [s1, s2] = [winnr(), getpos(".")]
#|         sil make | cope
#|         while winnr() != s1 | exec "norm! \<c-w>w" | endwh | call setpos(".", s2)
#|     elseif !a:nomove
#|         try | cnext | catch | cfirst | endtry
#|     endif
#|     " For neovim, check vim.diagnostic.{show,fromqflist}, vim.system, vim.schedule
#| endf
#- }}}
