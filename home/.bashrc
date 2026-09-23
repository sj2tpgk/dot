# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## Environment variables

export PATH=~/bin:~/.local/bin:$PATH

if command -v vim > /dev/null; then
    export EDITOR=vim
    export VISUAL=vim
elif command -v nano > /dev/null; then
    export EDITOR=nano
    export VISUAL=nano
fi

export GOPATH=~/.go
export PATH=$PATH:$GOPATH/bin

export RLWRAP_HOME=~/.config/rlwrap


## Aliases and functions

alias ls='ls --color=auto'
alias cp='cp -i' # safe cp

function mkcd(){
    mkdir -p "$1"
    cd "$1"
}


## Greeting

function greeting(){ date +"%Y-%m-%d (%a) %H:%M:%S  ($USER@$HOSTNAME)  Welcome to bash."; }
greeting
unset -f greeting


## Prompt

# escape sequences must be surrounded by \[ and \] (otherwise you get incorrect cursor position)
PS1='\[\e[1;33m\]`(myprompt1 -- --no-color 2>/dev/null || echo "${USER::5}@${HOSTNAME::5} ${PWD/$HOME/"~"} >>> ")`\[\e[0m\]'


## Misc

# set -o vi  # Enable vi-editing-mode (cursor settings in .inputrc)

