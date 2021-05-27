# Path
set -xg PATH $HOME/bin $PATH

# Keyboard
if [ (cat /etc/machine-id | md5sum | cut -c1-4) = f63a ]
    set -xg MYKBD "colemakdh"
end

# Alias
alias cp 'cp -i'
alias vim nvim

# Pager ( -x (--export) is necessary )
if command -v vimpager > /dev/null
    set -xg PAGER vimpager
    set -xg VIMPAGER_RC ~/.vimpagerrc
else if command -v w3mman > /dev/null
    set -xg PAGER w3_pager
    alias man w3_man
end

# Editor
if command -v emacs > /dev/null
    set -xg EDITOR ec
    set -xg VISUAL ec
else if command -v vim > /dev/null
    set -xg EDITOR vim
    set -xg VISUAL vim
else
    set -xg EDITOR nano
    set -xg VISUAL nano
end

# Rlwrap
set -xg RLWRAP_HOME ~/.rlwrap/

# Binding (binding for sudo is builtin)

fzf_key_bindings

bind \eg "commandline -r (commandline -b | sed 's/\s*\$/ | grep -i /')"
bind \eh "commandline -r (commandline -b | sed 's/\s*\$/ --help/')"
# bind \ed "commandline -r (find . -type d -iname '*(commandline -b | sed s/\///g)*' | fzy)"
bind -k ppage prevd-or-backward-word
bind -k npage nextd-or-forward-word

source ~/.config/fish/smart_jump.fish
# bind \cf forward-jump
# bind \ef backward-jump
# bind \ct repeat-jump
# bind \et repeat-jump-reverse

# bind \ed my_fish_fzy
# bind \ef "commandline -r hello"
# bind -k \cleft "commandline -r hello"

# function my_fish_fzy
#   set -l str1 (commandline -b | sed 's/\///g')
#   set -l str2 (find . -type d -iname '*'$str1'*' | fzy)
#   commandline -r $str2
#   commandline -f repaint
# end

# Git prompt: use ascii chars
set __fish_git_prompt_char_cleanstate       'cln'  # (Variable: ✔)
set __fish_git_prompt_char_conflictedstate  'x_x'  # (Variable: ✖)
set __fish_git_prompt_char_dirtystate       'chg'  # (Variable: ✚)
set __fish_git_prompt_char_stagedstate      'stg'  # (Variable: ●)
set __fish_git_prompt_char_untrackedfiles   'utr'  # (Variable: …)
set __fish_git_prompt_char_upstream_ahead   '+'    # (Variable: ↑)
set __fish_git_prompt_char_upstream_behind  '-'    # (Variable: ↓)
set __fish_git_prompt_char_upstream_prefix  ''     # (Variable: '')


set fish_color_autosuggestion 'magenta'
# set fish_color_cancel:\x2dr
set fish_color_command        'cyan'
set fish_color_comment        'magenta'
# set fish_color_cwd:green
# set fish_color_cwd_root:red
# set fish_color_end:009900
set fish_color_error          'red'
# set fish_color_escape:00a6b2
# set fish_color_history_current:\x2d\x2dbold
# set fish_color_host:normal
# set fish_color_host_remote:yellow
# set fish_color_match:\x2d\x2dbackground\x3dbrblue
# set fish_color_normal:normal
# set fish_color_operator:00a6b2
# set fish_color_param:00afff
set fish_color_quote          'yellow'
# set fish_color_redirection:00afff
# set fish_color_search_match:bryellow\x1e\x2d\x2dbackground\x3dbrblack
# set fish_color_selection:white\x1e\x2d\x2dbold\x1e\x2d\x2dbackground\x3dbrblack
# set fish_color_status:red
# set fish_color_user:brgreen
# set fish_color_valid_path:\x2d\x2dunderline
