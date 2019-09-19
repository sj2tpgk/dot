# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Environment variables {{{
PATH=$PATH:~/bin
if command -v vim > /dev/null; then
    export EDITOR=vim
    export VISUAL=vim
else
    export EDITOR=nano
    export VISUAL=nano
fi
# }}}

# Aliases and functions {{{
alias ls='ls --color=auto'
alias cp='cp -i' # safe cp
alias rg='ranger'

function mkcd(){
    mkdir -p "$1"
    cd "$1"
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
## }}}

## Prompt {{{
# get current branch in git repo
function parse_git_branch() {
	BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
	if [ ! "${BRANCH}" == "" ]
	then
		STAT=`parse_git_dirty`
		echo "(${BRANCH}${STAT}) "
	else
		echo ""
	fi
}

# get current status of git repo
function parse_git_dirty {
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

function genprompt {
  local s_path="36"
  local s_usr="32"
  local s_git="35"
  local s_arrow="1;33"

  local p_path="\[\e[${s_path}m\]\w\[\e[m\]"
  local p_usr="\[\e[${s_usr}m\](\u@\h)\[\e[m\]"
  local p_git="\[\e[${s_git}m\]\`parse_git_branch\`\[\e[m\]"
  local p_arrow="\[\e[1;33m\]>>>\[\e[m\]"

  echo "$p_path $p_usr $p_git$p_arrow "
}

export PS1=$(genprompt)
# export PS1="\[\e[36m\]\w\[\e[m\] \[\e[32m\](\u\[\e[m\]@\[\e[32m\]\h)\[\e[m\] \[\e[35m\]\`parse_git_branch\`\[\e[m\]\[\e[1;33m\]>>>\[\e[m\] "
# }}}
