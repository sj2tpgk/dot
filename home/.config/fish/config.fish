# TODO keybinding: right arrow to complete next word or forward-char, go to beginning or end of prev word (example: !echo! !yes! !no! (! is cursor positions)).

# Misc {{{

# Path
set -xg PATH $HOME/bin $PATH
set -xg PATH $HOME/localbin $PATH
set -xg PATH $HOME/.local/bin $PATH
set -xg SHELL /usr/bin/fish

# Golang
set -xg GOPATH $HOME/.go
set -xg PATH   $GOPATH/bin $PATH

# Keyboard
# if [ (machineid) = c0c2 ]
#     set -xg MYKBD "colemakdh"
# end

# Helpers
function has; command -v $argv[1] >/dev/null 2>&1; end

# Alias
function aliasif; has $argv[2] && alias $argv[1] $argv[2]; end
alias   cp   'cp -i'
aliasif gstd gst-discoverer-1.0
aliasif gsti gst-inspect-1.0
aliasif gstl gst-launch-1.0
aliasif js   node
aliasif nv   nvim
aliasif v    nvim
aliasif sudo doas
aliasif ra   ranger
has rlwrap && alias sh 'PS1="\$ " rlwrap -p"3;34" sh' # Dash has no history, arrow keys etc.

# Abbr
if has sudo
    abbr s    sudo
end
if has pacman
    abbr p         pacman
    abbr pqi       pacman -Qi
    abbr pql       pacman -Ql
    abbr pqs       pacman -Qs
    abbr pi   sudo pacman -S
    abbr pss       pacman -Ss
    abbr psu  sudo pacman -Su
    abbr psy  sudo pacman -Sy
    abbr pr   sudo pacman -Rs
end
if has git
    abbr ga   git add .
    abbr gc   git commit -m
    abbr gd   git diff
    abbr gps  git push origin master
    abbr gpu  git pull origin master
    abbr gs   git status
end
if has flatpak
    abbr fl        flatpak
    abbr fli  sudo flatpak install
    abbr flr       flatpak run
    abbr fls       flatpak search
    abbr flui      flatpak uninstall
    abbr flup      flatpak update
end

# Completion
# "quiet" : completion like "if" or "sudo" (followed by another commands)
complete -c quiet -xa '(__fish_complete_subcommand)'
complete -c q     -xa '(__fish_complete_subcommand)'

# Pager and Man ( -x (--export) is necessary )
if has vimpager
    set -xg PAGER vimpager
    set -xg VIMPAGER_RC ~/.vimpagerrc
else if has w3_pager && has w3_man
    set -xg PAGER w3_pager
    alias man w3_man
else if has w3m && has w3mman
    set -xg PAGER w3m
    alias man w3mman
end

# Editor
# if has emacs
#     set -xg EDITOR ec
#     set -xg VISUAL ec
if has nvim
    set -xg EDITOR nvim
    set -xg VISUAL nvim
else if has vim
    set -xg EDITOR vim
    set -xg VISUAL vim
else if has nano
    set -xg EDITOR nano
    set -xg VISUAL nano
end

# Rlwrap
set -xg RLWRAP_HOME ~/.rlwrap/

# Command not found
function fish_command_not_found --on-event fish_command_not_found
    __fish_default_command_not_found_handler $argv
    if string match -r '^\w' $argv[1] >/dev/null
        if command -v pacman >/dev/null
            # pacman -F $argv[1] # simple
            # echo "pacman -F $argv[1]" >&2
            # pacman -F $argv[1] | awk '/^[^ ]/{p=$0;sub(" .*","",p)}/^ /&&/bin/{cmd=$0;sub("^.*/","",cmd);print cmd "\t in " p}' >&2
            set reg (echo $argv[1] | awk '{s=$0;n=length($0);p=s;  for(i=1;i<=n;i++)p=p"|"substr(s,1,i-1)".?"substr(s,i+1,n-i);  for(i=0;i<=n;i++)p=p"|"substr(s,1,i)"."substr(s,i+1,n-i);  print "^("p")$"}')
            echo "pacman -Fx '$reg'" >&2
            pacman -Fx $reg 2>/dev/null | awk '/^[^ ]/{p=$0;sub(" .*","",p)}/^ /&&/bin/{cmd=$0;sub("^.*/","",cmd);print cmd "\t in " p}' >&2
        end
    end
end

# }}}

# Key bindings {{{

type -q fzf_key_bindings && fzf_key_bindings

# complete one WORD when end of line, otherwise forward-char
# "commandline -b" may print multiple numbers. "head -n 1" is a temporary workaround
bind \e\[C "if [ (commandline -C) = (commandline -b | string length | head -n 1) ]; commandline -f forward-bigword; commandline -f repaint; else; commandline -f forward-char; end"
bind \ei 'commandline -r "function f; "(commandline -b | awk "BEGIN{i=0}{while(sub(/--/,\"\$argv[\"i\"]\")){i++}print}")"; end"'

# \es for sudo is builtin, but I enhance it
bind \es "commandline -r (commandline -b | sed 's#\s*\$##; s#^\s*#sudo #; s#pacman -Ss#pacman -S#')"
bind \eg "commandline -r (commandline -b | sed 's#\s*\$# | grep -i #')"
bind \eh "commandline -r (commandline -b | sed 's#\s*\$# --help#')"
bind \em popup_help_man
bind -k ppage prevd-or-backward-word
bind -k npage nextd-or-forward-word
bind -k btab  nextd-or-forward-word # shift+tab

# Unbind function keys (use fish_key_reader to find out key name)
for i in f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12; bind -k $i ""; end
for i in \e\[25\;2~ \e\[26\;5~; bind $i ""; end

# bind \ed my_fish_fzy
# bind \ef "commandline -r hello"
# bind -k \cleft "commandline -r hello"

# function my_fish_fzy
#   set -l str1 (commandline -b | sed 's/\///g')
#   set -l str2 (find . -type d -iname '*'$str1'*' | fzy)
#   commandline -r $str2
#   commandline -f repaint
# end

# }}}

# Smart jump (alternative for forward-jump etc.) {{{

# Get time in milli seconds
function jump_gettime; set s (date +%s%N); echo (string sub -l (math (string length $s) - 6) $s); end

# If called within cooldown, run repeat command
set jump_cooldown 1600
# The time smart_jump was last used.
set jump_lasttime (math (jump_gettime) - $jump_cooldown)
# Last jump was forward or backward?
set jump_cmd none

function smart_jump -d 'Jump like vim-snipe.'
    set -l now (jump_gettime)
    set -l cmd $argv[1]
    commandline -f backward-char
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
    commandline -f forward-char
    set jump_lasttime $now
end

bind \cf "smart_jump backward-jump"
bind \ef "smart_jump forward-jump"

# }}}

# Git prompt: use ascii chars {{{
set __fish_git_prompt_char_cleanstate       ''     # (Variable: ✔)
set __fish_git_prompt_char_conflictedstate  '?'    # (Variable: ✖)
set __fish_git_prompt_char_dirtystate       '+'    # (Variable: ✚)
set __fish_git_prompt_char_stagedstate      ' stg' # (Variable: ●)
set __fish_git_prompt_char_untrackedfiles   ' utr' # (Variable: …)
set __fish_git_prompt_char_upstream_ahead   '^'    # (Variable: ↑)
set __fish_git_prompt_char_upstream_behind  'v'    # (Variable: ↓)
set __fish_git_prompt_char_upstream_prefix  ''     # (Variable: '')
# }}}

# Colors {{{

set fish_color_autosuggestion 'magenta'
# set fish_color_cancel:\x2dr
set fish_color_command        'cyan'
set fish_color_comment        'magenta'
# set fish_color_cwd:green
# set fish_color_cwd_root:red
# set fish_color_end:009900
set fish_color_error          'red'
# set fish_color_escape         'blue'
# set fish_color_history_current:\x2d\x2dbold
# set fish_color_host:normal
# set fish_color_host_remote:yellow
# set fish_color_match:\x2d\x2dbackground\x3dbrblue
# set fish_color_normal:normal
set fish_color_operator       'blue'        # (~       in ~/xxx/yyy)
set fish_color_param          'blue'        # (xxx/yyy in ~/xxx/yyy)
set fish_color_quote          'yellow'
# set fish_color_redirection:00afff
set fish_color_redirection    'cyan'
# set fish_color_search_match:bryellow\x1e\x2d\x2dbackground\x3dbrblack
# set fish_color_selection:white\x1e\x2d\x2dbold\x1e\x2d\x2dbackground\x3dbrblack
# set fish_color_status:red
# set fish_color_user:brgreen
set fish_color_valid_path     '--underline' # (underline if file exist (color is set in _operator and _param)

# }}}

# Extra commands {{{

function mkcd -d 'mkdir plus cd'
  set -l dirname $argv[1]
  mkdir -p $dirname
  eval "cd" $dirname
end

function tmuxcd -d 'cd to pwd of tmux last-pane'
  # cd (tmux last-pane; tmux-path getpanepath; tmux last-pane)
  set -l dir (tmux display -p '#{TMUX_LAST_PATH}')
  # [ -z $dir ] && set -l dir (tmux last-pane; tmux display -p '#{pane_current_path}'; tmux last-pane)
  cd $dir
end

function rcd -d 'cd using ranger'
  set -l tmp (mktemp)
  ranger \
    --cmd "map q chain shell echo %d > '$tmp'; quitall" \
    --cmd "map Q chain shell echo %d > '$tmp'; quitall"
  cat $tmp | read -l dir
  [ -d $dir ] && cd $dir
end
alias rangercd rcd

function popup_help_man
  set -l cmd (commandline -b | sed "s/ .*//")
  [ -z $popup_help_man_last_cmd ] && set -g popup_help_man_last_cmd $cmd
  [ -z $cmd ] && return
  if [ $cmd != $popup_help_man_last_cmd ]
    tmux popup -E -h 80% -b heavy -s fg=green sh -c "{ $SHELL -c '$cmd --help' || $SHELL -c '$cmd -h'; } 2>&1 | $PAGER"
  else
    tmux popup -E -h 80% -b heavy -s fg=green man $cmd
  end
  set -g popup_help_man_last_cmd $cmd
end

# }}}

# Greeting {{{

function fish_greeting
  switch (date "+%a")
    case 土
      echo -n (set_color yellow)(date "+%Y-%m-%d") (set_color green)\((set_color blue)(date "+%a")(set_color green)\) (set_color cyan)(date "+%H:%M:%S")(set_color white)
    case 日
      echo -n (set_color yellow)(date "+%Y-%m-%d") (set_color green)\((set_color red)(date "+%a")(set_color green)\) (set_color cyan)(date "+%H:%M:%S")(set_color white)
    case '*'
      echo -n (set_color yellow)(date "+%Y-%m-%d") (set_color green)\((set_color white)(date "+%a")(set_color green)\) (set_color cyan)(date "+%H:%M:%S")(set_color white)
  end

  echo "  Welcome to" (set_color cyan)fish (set_color blue)'<\'))><'(set_color white)
end

# }}}

# Prompt {{{

begin
  set __myfish_myprompt (command -v myprompt)
  if test -n "$__myfish_myprompt" # quoting is important here
    set __myfish_mawk (command -v mawk)
    function fish_prompt
      set -l laststatus  $status
      set -l lastkillsig $fish_kill_signal
      if test -n "$fish_private_mode" # quoting is important here
        # myprompt.sh fish_private $laststatus $lastkillsig
        if test -n "$__myfish_mawk" # quoting is important here
          $__myfish_mawk -f $__myfish_myprompt -- --shell fish_private \
                         --last-status $laststatus --last-signal $lastkillsig
        else
          $__myfish_myprompt                   -- --shell fish_private \
                         --last-status $laststatus --last-signal $lastkillsig
        end
      else
        # myprompt.sh fish         $laststatus $lastkillsig
        if test -n "$__myfish_mawk" # quoting is important here
          $__myfish_mawk -f $__myfish_myprompt -- --shell fish \
                         --last-status $laststatus --last-signal $lastkillsig
        else
          $__myfish_myprompt                   -- --shell fish \
                         --last-status $laststatus --last-signal $lastkillsig
        end
      end
    end
  end
end

# }}}


# vim: fdm=marker cms=#%s
