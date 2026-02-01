sh ~/script/x-setting.sh &
# command -v python > /dev/null && python ~/script/mouse-digio-fix.py &

# # used in .tmux.conf .vimrc etc.
# xrdb ~/.Xresources
# setxkbmap -layout jp
# # xset r rate 210 38
# xset r rate 180 45
# xset b off
# xset s off -dpms          # Don't use dpms for screen timeout
# xset s 7200 7200          # Specify screen timeout here
# xset dpms 1800 2700 3600  # DPMS standby/suspend/off time (?)

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
# Now we set QT_QPA_PLATFORMTHEME when launching each application. Only set a fallback here.
# qt5
# export QT_QPA_PLATFORMTHEME=qt5ct
# export QT_STYLE_OVERRIDE=adwaita
# qt6; need aur/qt6gtk2;
# export QT_QPA_PLATFORMTHEME=qt6gtk2
# export DESKTOP_SESSION=gnome
# export QT_QPA_PLATFORMTHEME=gtk2
export QT_QPA_PLATFORMTHEME=qt5ct
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
# /usr/bin/syncthing & # use `systemctl --user enable --now syncthing`

# Auto mount USB etc.
udiskie --no-automount &

# Pulseaudio
# /usr/bin/start-pulseaudio-x11

# exec openbox
# exec icewm-session
# exec icewm
MYKBD=colemakdh SHELL=/bin/sh exec startfluxbox    # ~/.fluxbox/startup  will be executed
# exec pekwm
# setxkbmap -layout "us,jp" && exec awesome
# exec i3 -V >> ~/.config/i3/i3log-$(date +'%F-%k-%M-%S') 2>&1
# exec i3
# exec startxfce4
# exec fvwm
# exec fvwm3
# exec enlightenment_start

# vim:ft=sh
