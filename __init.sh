#!/bin/sh

DOTDIR=~/dot

ignoreall=0  # ignore all confliction

safelink(){
  # usage: safelink <targetPath> <linkPath>
  # if linkPath already exists, choose from [o]verride, [o]ackup, [s]kip

  local tgtpath="$1"
  local lnpath="$2"

  local force=0      # make symlink to targetPath at linkPath (force)
  local backup=0     # backup original file whose path is linkPath

  if [ ! -f "$lnpath" -o $ignoreall -eq 1 ]; then
    # linkPath does not exist (or ignoreall=1)
    force=1
  else
    [ -L "$lnpath" ] && echo -n "Symlink" || echo -n "File"
    echo " $lnpath already exists."

    read -p "[i]gnoreall, [o]verride, [b]ackup or [s]kip: " selection

    case "$selection" in
      i) ignoreall=1 ;;
      o) force=1 ;;
      b) backup=1 ;;
      s) ;;
      *) echo "Invalid selection."; exit 1 ;;
    esac
  fi

  if [ $force -eq 1 ]; then
    [ ! -d $(dirname $lnpath) ] && mkdir -p "$(dirname $lnpath)"
    if [ -d "$lnpath" ]; then
      # Doing "ln -s X Y" with Y a directory creates a link IN Y, rather than
      # making Y a link to X. Avoid this by removing Y first.
      rm -r "$lnpath"
    fi
    ln -s -f "$tgtpath" "$lnpath"
  elif [ $backup -eq 1 ]; then
    mv "$lnpath" "$lnpath_$(date +%y%m%d_%H%M%S)"
    ln -s "$tgtpath" "$lnpath"
  fi

}

safelink $DOTDIR/.bashrc             ~/.bashrc
safelink $DOTDIR/.inputrc            ~/.inputrc
safelink $DOTDIR/.nanorc             ~/.nanorc
safelink $DOTDIR/.tmux.conf          ~/.tmux.conf
safelink $DOTDIR/.vimrc              ~/.vimrc

safelink $DOTDIR/bin/cl              ~/bin/cl
safelink $DOTDIR/bin/du1             ~/bin/du1
safelink $DOTDIR/bin/en              ~/bin/en
safelink $DOTDIR/bin/findis          ~/bin/findis
safelink $DOTDIR/bin/gir             ~/bin/gir
safelink $DOTDIR/bin/img2txtfull     ~/bin/img2txtfull
safelink $DOTDIR/bin/nf1             ~/bin/nf1
safelink $DOTDIR/bin/passe           ~/bin/passe
safelink $DOTDIR/bin/randomstr       ~/bin/randomstr
safelink $DOTDIR/bin/running         ~/bin/running
safelink $DOTDIR/bin/scheme          ~/bin/scheme
safelink $DOTDIR/bin/scm             ~/bin/scm
safelink $DOTDIR/bin/w3              ~/bin/w3

safelink $DOTDIR/fish/config.fish    ~/.config/fish/config.fish
safelink $DOTDIR/fish/functions      ~/.config/fish/functions

safelink $DOTDIR/ranger/commands.py  ~/.config/ranger/commands.py
safelink $DOTDIR/ranger/rc.conf      ~/.config/ranger/rc.conf
safelink $DOTDIR/ranger/rifle.conf   ~/.config/ranger/rifle.conf
safelink $DOTDIR/ranger/scope.sh     ~/.config/ranger/scope.sh

safelink $DOTDIR/w3m/config          ~/.w3m/config
safelink $DOTDIR/w3m/keymap          ~/.w3m/keymap
safelink $DOTDIR/w3m/search.html     ~/.w3m/search.html

# Termux
safelink $DOTDIR/termux/colors.properties  ~/.termux/colors.properties
safelink $DOTDIR/termux/termux.properties  ~/.termux/termux.properties
safelink $DOTDIR/termux/termux_setup.sh    ~/.termux/termux_setup.sh

# Graphical
safelink $DOTDIR/i3/config           ~/.config/i3/config
