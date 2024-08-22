# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Environment variables {{{
PATH=$PATH:~/bin
PATH=$PATH:~/localbin
PATH=$PATH:~/.local/bin
if command -v vim > /dev/null; then
    export EDITOR=vim
    export VISUAL=vim
else
    export EDITOR=nano
    export VISUAL=nano
fi

export RLWRAP_HOME=~/.rlwrap

export GOPATH=~/.go
PATH=$PATH:$GOPATH/bin

export RLWRAP_HOME=~/.config/rlwrap


# Keyboard
if [ $(cat /etc/machine-id | md5sum | cut -c1-4) = f63a ]; then
    export MYKBD colemakdh
fi

# }}}

# Aliases and functions {{{
alias ls='ls --color=auto'
alias cp='cp -i' # safe cp

function mkcd(){
    mkdir -p "$1"
    cd "$1"
}

function tmuxcd(){
    cd "$(tmux last-pane; tmux display -pF '#{pane_current_path}'; tmux last-pane)"
}
# }}}

## Greeting {{{
function greeting(){
    local date=$(date +"%Y-%m-%d")
    local day_idx=$(date +"%w") # 0=Sun. .. 6=Sat.
    local day_color=""
    case $day_idx in
        0)  day_color=31 ;;
        6)  day_color=34 ;;
    esac
    local day="\e[${day_color}m$(date +%a)"
    local time=$(date +"%H:%M:%S")

    echo -ne "\e[33m$date \e[32m($day\e[32m) \e[36m$time\e[m"
    echo -n  "  "
    echo -ne "\e[34m($USER\e[m@\e[34m$HOSTNAME)\e[m"
    echo -n  "  "
    echo -n  "Welcome to bash."
    echo
}

greeting
unset -f greeting
## }}}

## Prompt {{{
# get current branch in git repo
function __ps1_parse_git_branch() {
	BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
	if [ ! "${BRANCH}" == "" ]
	then
		STAT=`__ps1_parse_git_dirty`
		echo "(${BRANCH}${STAT}) "
	else
		echo ""
	fi
}

# get current status of git repo
function __ps1_parse_git_dirty {
	status=`git status 2>&1 | tee`
	dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
	untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
	ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
	newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
	renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
	deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
	bits=''
	if [ "${renamed}" == "0" ]; then
		bits=">${bits}"
	fi
	if [ "${ahead}" == "0" ]; then
		bits="*${bits}"
	fi
	if [ "${newfile}" == "0" ]; then
		bits="+${bits}"
	fi
	if [ "${untracked}" == "0" ]; then
		bits="?${bits}"
	fi
	if [ "${deleted}" == "0" ]; then
		bits="x${bits}"
	fi
	if [ "${dirty}" == "0" ]; then
		bits="!${bits}"
	fi
	if [ ! "${bits}" == "" ]; then
		echo " ${bits}"
	else
		echo ""
	fi
}

function __ps1_arrow3_color {
  # -n not work ?
  if [ -z $RANGER_LEVEL ]; then
    if [ -z $NNNLVL ]; then
      echo -n "31"
    else
      echo -n "36"
    fi
  else
    echo -n "36"
  fi
}

function genprompt {
  local s_path="36"
  local s_usr="32"
  local s_git="35"

  local p_path="\[\e[${s_path}m\]\w\[\e[m\]"
  local p_usr="\[\e[${s_usr}m\](\`whoami | cut -c-2\`@${HOSTNAME:0:2})\[\e[m\]"
  # Git prompt is slow.
  # local p_git="\[\e[${s_git}m\]\`__ps1_parse_git_branch\`\[\e[m\]"
  local p_arrow="\[\e[1;33m\]>>\[\e[\`__ps1_arrow3_color\`m\]>\[\e[m\]"

  echo "$p_path $p_usr $p_git$p_arrow "
}

export PS1=$(genprompt)
unset -f genprompt

# export PS1="\[\e[36m\]\w\[\e[m\] \[\e[32m\](\u\[\e[m\]@\[\e[32m\]\h)\[\e[m\] \[\e[35m\]\`parse_git_branch\`\[\e[m\]\[\e[1;33m\]>>>\[\e[m\] "
# }}}

# set -o vi  # Enable vi-editing-mode (cursor settings in .inputrc)

# nnn {{{

if command -v nnn > /dev/null; then

  export NNN_USE_EDITOR=1          # use the $EDITOR to open text files
  export NNN_CONTEXT_COLORS="2136" # use a different color for each context

  nn () {
    # Block nesting of nnn in subshells
    if [ -n "$NNNLVL" ] && [ "${NNNLVL:-0}" -ge 1 ]; then
      echo "nnn is already running"
      return
    else

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
      . "$NNN_TMPFILE"
      rm -f "$NNN_TMPFILE" > /dev/null
    fi
    fi
  }

fi

# }}}

# source /home/tpat3/.config/broot/launcher/bash/br


# JINA_CLI_BEGIN

## autocomplete
_jina() {
  COMPREPLY=()
  local word="${COMP_WORDS[COMP_CWORD]}"

  if [ "$COMP_CWORD" -eq 1 ]; then
    COMPREPLY=( $(compgen -W "$(jina commands)" -- "$word") )
  else
    local words=("${COMP_WORDS[@]}")
    unset words[0]
    unset words[$COMP_CWORD]
    local completions=$(jina completions "${words[@]}")
    COMPREPLY=( $(compgen -W "$completions" -- "$word") )
  fi
}

complete -F _jina jina

# session-wise fix
ulimit -n 4096
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
# default workspace for Executors

# JINA_CLI_END
