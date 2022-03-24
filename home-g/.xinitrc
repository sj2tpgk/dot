# used in .tmux.conf .vimrc etc.
xrdb ~/.Xresources
setxkbmap -layout jp
# xset r rate 210 38
xset r rate 180 45
xset b off
xset s off -dpms          # Don't use dpms for screen timeout
xset s 7200 7200          # Specify screen timeout here
xset dpms 1800 2700 3600  # DPMS standby/suspend/off time (?)

dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY

## Japanese IME {{{
# -- only works when in .xinitrc ?
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &
export XMODIFIERS='@im=uim'
# uim-toolbar-gtk-systray &

# export GTK_IM_MODULE='fcitx'
# export QT_IM_MODULE='fcitx'
# export XMODIFIERS='@im=fcitx'
# fcitx-autostart &

# export GTK_IM_MODULE='ibus'
# export QT_IM_MODULE='ibus'
# export XMODIFIERS='@im=ibus'
# ibus-daemon -drx &

# export XMODIFIERS=@im=SCIM
# export GTK_IM_MODULE="scim"
# export QT_IM_MODULE="scim"
# scim -d &
# }}}

## Environment variables (mustn't be set in wminit.sh ???)
export QT_QPA_PLATFORMTHEME=qt5ct
# export DESKTOP_SESSION=gnome
export QT_QPA_PLATFORMTHEME=gtk2
export TZ=Asia/Tokyo
export EDITOR='vim'

export MYKBD="colemakdh"

# no gtk3 scrollbar fade
export GTK_OVERLAY_SCROLLING=0


# wminit
sh ~/script/wminit.sh

# amixer set PCM 182
amixer set PCM 50%
amixer set Master 40%

# Cloud storage
# /usr/bin/dropbox &
/usr/bin/syncthing &

# Auto mount USB etc.
udiskie &

# Pulseaudio
# /usr/bin/start-pulseaudio-x11

# exec openbox
# exec icewm-session
# exec icewm
exec startfluxbox    # ~/.fluxbox/startup  will be executed
# exec pekwm
# setxkbmap -layout "us,jp" && exec awesome
# exec i3 -V >> ~/.config/i3/i3log-$(date +'%F-%k-%M-%S') 2>&1
# exec i3
# exec startxfce4
# exec fvwm
# exec fvwm3
# exec enlightenment_start

# vim:ft=sh
