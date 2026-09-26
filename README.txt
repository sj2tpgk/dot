To install:

./install.sh home ~
./install.sh home-g ~ (optional, for gui)
./install.sh extra/home-termux ~ (optional, for termux)

Tmux virtual env:

tmux2 is a shell script with config of command line tools such as shell and text editors and some other shell scripts embedded. It works like a virtual env (extracts them to a temp directory on start and erases them on close).

To regenerate: ./tmux pack

To download:

curl --compressed "https://codeberg.org/sj2tpgk/dot/raw/branch/master/tmux2" > ~/tmux && chmod +x ~/tmux
wget --compression=auto "https://codeberg.org/sj2tpgk/dot/raw/branch/master/tmux2" -O ~/tmux && chmod +x ~/tmux
