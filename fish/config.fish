# Path
set -xg PATH $HOME/bin $PATH

# Alias
alias cp 'cp -i'
alias rg ranger

# Pager ( -x (--export) is necessary )
if command -v vimpager > /dev/null
  set -xg PAGER vimpager
  set -xg VIMPAGER_RC ~/.vimpagerrc
end

# Editor
if command -v emacs > /dev/null
    set -xg EDITOR en
    set -xg VISUAL en
else if command -v vim > /dev/null
    set -xg EDITOR vim
    set -xg VISUAL vim
else
    set -xg EDITOR nano
    set -xg VISUAL nano
end
