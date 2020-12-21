#!/bin/sh

# update myazik rule
# cwd=$(pwd)
# cd ~/.uim.d/myazik/
# sh all.sh
# cd "$cwd"


pkill uim-xim
pkill uim-toolbar-gtk
pkill uim-helper-serv

sleep 0.8

export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &
export XMODIFIERS='@im=uim'
# uim-toolbar-gtk-systray &
sleep 0.3
