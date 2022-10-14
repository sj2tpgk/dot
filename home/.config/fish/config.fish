# TODO keybinding: right arrow to complete next word or forward-char, go to beginning or end of prev word (example: !echo! !yes! !no! (! is cursor positions)).

# Misc {{{

# Path
set -xg PATH $HOME/bin $PATH
set -xg PATH $HOME/localbin $PATH
set -xg PATH $HOME/.local/bin $PATH

# Keyboard
if [ (machineid) = c0c2 ]
    set -xg MYKBD "colemakdh"
end

# Helpers
function has; command -v $argv[1] >/dev/null 2>&1; end

# Alias
function aliasif; has $argv[2] && alias $argv[1] $argv[2]; end
alias   cp   'cp -i'
aliasif gstd gst-discoverer-1.0
aliasif gsti gst-inspect-1.0
aliasif gstl gst-launch-1.0
aliasif nv   nvim
aliasif rg   ranger

# Completion
# "quiet" : completion like "if" or "sudo" (followed by another commands)
complete -c quiet -xa '(__fish_complete_subcommand)'

# Pager ( -x (--export) is necessary )
if has vimpager
    set -xg PAGER vimpager
    set -xg VIMPAGER_RC ~/.vimpagerrc
else if has w3mman -v w3mman
    set -xg PAGER w3_pager
    alias man w3_man
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

# }}}

# Key bindings {{{

type -q fzf_key_bindings && fzf_key_bindings

# \es for sudo is builtin
bind \eg "commandline -r (commandline -b | sed 's/\s*\$/ | grep -i /')"
bind \eh "commandline -r (commandline -b | sed 's/\s*\$/ --help/')"
bind -k ppage prevd-or-backward-word
bind -k npage nextd-or-forward-word
bind -k btab  nextd-or-forward-word # shift+tab

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
  set dirname $argv[1]
  mkdir -p $dirname
  eval "cd" $dirname
end

function tmuxcd -d 'cd to pwd of tmux last-pane'
  cd (tmux last-pane; tmux-path getpanepath; tmux last-pane)
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

function fish_prompt
  set laststatus $status
  set lastkillsig $fish_kill_signal
  if test -n "$fish_private_mode"
    myprompt fish_private $laststatus $lastkillsig
  else
    myprompt fish         $laststatus $lastkillsig
  end
end

# }}}


# vim: set fdm=marker cms=#%s
