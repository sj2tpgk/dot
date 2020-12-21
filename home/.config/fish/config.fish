# Path
set -xg PATH $HOME/bin $PATH

# Keyboard
set -xg MYKBD "colemakdh"

# Alias
alias cp 'cp -i'
alias rg ranger

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
