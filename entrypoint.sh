#!/bin/sh

# Usage:
# sudo docker run --rm -it -e HOST_UID=$(id -u) -e HOST_GID=$(id -g) -v (pwd):/m mydev

[ -z "$HOST_UID" ] && echo "HOST_UID is not set! Files you create in mounted directories may have root permission!"
[ -z "$HOST_GID" ] && echo "HOST_GID is not set! Files you create in mounted directories may have root permission!"

USER_ID=${HOST_UID:-1000}
GROUP_ID=${HOST_GID:-1000}

user=user

echo "Using UID=$USER_ID, GID=$GROUP_ID"
usermod -u "$USER_ID" "$user"
groupmod -g "$GROUP_ID" "$user"

/usr/sbin/tmux
