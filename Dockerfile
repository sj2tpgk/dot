FROM archlinux:latest

# Chaotic-AUR
RUN pacman-key --init && \
        pacman-key --recv-key FBA220DFC880C036 --keyserver keyserver.ubuntu.com && \
        pacman-key --lsign-key FBA220DFC880C036 && \
        pacman --noconfirm -U 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-keyring.pkg.tar.zst' 'https://cdn-mirror.chaotic.cx/chaotic-aur/chaotic-mirrorlist.pkg.tar.zst' && \
        printf "\n\n[chaotic-aur]\nInclude = /etc/pacman.d/chaotic-mirrorlist" >> /etc/pacman.conf

# Upgrade system and install packages
RUN pacman --noconfirm -Syu
RUN pacman --noconfirm -S \
        bash git neovim fish ranger tmux entr fzf w3m \
        curl wget \
        lua perl \
        shellcheck bash-language-server \
        nodejs typescript-language-server \
        python pyright

# Expose some ports to host by default.
EXPOSE 8000 8001 8080 8081 8082 8083 8084 8085

# Should mount current dir to /m
WORKDIR /m

# Allow su command
RUN usermod -p pass root

# Create new user "user", set empty password, and copy config.
RUN useradd -m user && passwd -d user
COPY home/ /home/user/
RUN chown -R user /home/user/
USER user

# Set ENTRYPOINT, it should use usermod and groupmod to match host's UID and GID
COPY entrypoint.sh /
ENTRYPOINT sh /entrypoint.sh

# TODO tmux shell fish???
