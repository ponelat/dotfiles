# Add Python installed bins to path
export PATH+=:"$HOME/.local/bin"

# Linuxbrew, see: 
export PATH="$HOME/.linuxbrew/bin:$PATH"
export MANPATH="$HOME/.linuxbrew/share/man:$MANPATH"
export INFOPATH="$HOME/.linuxbrew/share/info:$INFOPATH"
export EDITOR=vim

# Linux brew bash completions
for com in "$HOME/.linuxbrew/etc/bash_completion.d/*"; do
  . $com
done

# Setup color support
# export TERM=xterm-256color
case "$TERM" in
       xterm*) TERM=xterm-256color
esac

# Base16 Shell
BASE16_SHELL="/home/josh/.config/base16-shell/base16-google.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

eval `dircolors ~/.dircolors`

# git  helpers aliases and functions
source "/home/josh/.githelpers"

# git flow autocompletion
# source "$HOME/git-flow-completion.bash"

# LiquidPrompt
source "$HOME/liquidprompt/liquidprompt"
# config will be in ~/.config/liquidprompt.sh

# Autojump (easier file jumping)
source /usr/share/autojump/autojump.sh

# Powerline
# powerline-daemon -q # this should be hiding in ~/.local/bin
# POWERLINE_BASH_CONTINUATION=1
# POWERLINE_BASH_SELECT=1
# source "$HOME/.local/lib/python2.7/site-packages/powerline/bindings/shell/powerline.sh"

# Aliases 
alias ls='ls -A --color=always --group-directories-first -1 -v'
alias o='xdg-open'
alias .b=". ~/dotfiles/.bashrc"
alias .color=". ~/.config/base16-shell/colortest"
alias vib="vi ~/dotfiles/.bashrc"
alias dotinstall=". ~/dotfiles/install.sh"

alias npm-exec='PATH=$(npm bin):$PATH'

# The following function has been moved into it's own file...
# Reboot into windows...
# function windows() {
#   # For this to work without requiring a password, run this...
#   # sudo visudo
#   # then add this to the bottom of sudoers
#   # josh    ALL=(ALL) NOPASSWD: /sbin/shutdown, /sbin/reboot, /sbin/poweroff, /usr/sbin/grub-reboot
#   sudo grub-reboot 4 && sudo reboot
# }

function be () {
  bundle exec "$*"
}

function lsla() {
  ls -p -cltr "$*" | grep -v /
}

# Some powerful fuzzyfinder shortcuts to help me move around
function s () {
  # see https://github.com/garybernhardt/selecta
  selecta
}

function ss() {
  cd $(find ~/projects -maxdepth 2 -type d | s) 
}

function cds() {
  limit=5
  # here we have a default value for our arg, go bash!
  # note: the hyphen is part of the syntax ....${var:-default}
  base=${1:-.}
  echo Limited to $limit
  cd $(find_dir_ignore_common "$base" -maxdepth $limit -type d | s) 
}

function cdd() {
  cd `print_parent_paths | s`
}

function count_parent_folders() {
 all=`tr '/' '\n' | wc -l `
 echo $[$all-1]
}

function print_parent_paths() {
  p=`pwd`
  PARENTS=`echo $p | count_parent_folders`
  COUNTER=0
  NUM=2

  while [[ $COUNTER -lt $PARENTS ]]; do
    echo `expr $p : '\(\([^/]*/\)\{'$[$PARENTS-$COUNTER]'\}\)'`
    COUNTER=$[$COUNTER+1]
  done

# expr /hello/bob/and/mary : '\(\([^/]*/\)\{4\}\)'
}

function cps() {
  limit=3
  src_base=${1:-.}
  dest_base=${2:-.}

  src_base_final="$(find_dir_ignore_common "$src_base" | s)"
  src=$(find $src_base_final -maxdepth $limit | s)
  dest=$(find_dir_ignore_common "$dest_base" | s)

  cp "$src" "$dest" 
}

function cpss() {
  src_base=${1:-~/projects}
  dest_base=${2:-.}
  cps "$src_base" "$dest_base"
}

function find_dir_ignore_common() {
  base=${1:-.}
  find $1 -type d \( ! -path "*node_modules*" ! -path '*bower_components*' ! -path '*git*' ! -path '*cache*' \)
}


# Run git status if in git repo, else ls -la
# see: https://gist.github.com/andrewberls/6119868#file-cs-sh
function cs {
  clear
  if ! git ls-files >& /dev/null
  then
    ls -la $1
  else
    git status $1
  fi
}

# NVM ----------------------------------------------------
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# Sauce Credentials -------------------------------------       
export SAUCE_USERNAME="ponelat"
export SAUCE_ACCESS_KEY="2b212c69-8a63-4846-b0af-32a7fc68950e"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# Gulp task autocompletion, needs to be after nvm setup
eval "$(gulp --completion=bash)"

# Grunt task autocompletion, needs to be after nvm setup
eval "$(grunt --completion=bash)"

###############################################################################
############ Github gits for ssh-agent
###############################################################################
# See: https://help.github.com/articles/working-with-ssh-key-passphrases#auto-launching-ssh-agent-on-msysgit
###############################################################################
# Note: ~/.ssh/environment should not be used, as it
#       already has a different purpose in SSH.

env=~/.ssh/agent.env

# Note: Don't bother checking SSH_AGENT_PID. It's not used
#       by SSH itself, and it might even be incorrect
#       (for example, when using agent-forwarding over SSH).

agent_is_running() {
    if [ "$SSH_AUTH_SOCK" ]; then
        # ssh-add returns:
        #   0 = agent running, has keys
        #   1 = agent running, no keys
        #   2 = agent not running
        ssh-add -l >/dev/null 2>&1 || [ $? -eq 1 ]
    else
        false
    fi
}

agent_has_keys() {
    ssh-add -l >/dev/null 2>&1
}

agent_load_env() {
    . "$env" >/dev/null
}

agent_start() {
    (umask 077; ssh-agent >"$env")
    . "$env" >/dev/null
}

if ! agent_is_running; then
    agent_load_env
fi

# if your keys are not stored in ~/.ssh/id_rsa.pub or ~/.ssh/id_dsa.pub, you'll need
# to paste the proper path after ssh-add
if ! agent_is_running; then
    agent_start
    ssh-add
elif ! agent_has_keys; then
    ssh-add
fi

unset env


# added by travis gem
[ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"


# Set vi mode by default, probably should be the last line
# ...I've made it the second last 'section' so that I can add a VI mode in the prompt
set -o vi
# set show-mode-in-prompt on

# Prompt
get_root_git_dir() {
    printf "%s" $(pwd | sed "s:$HOME:~:")
}

get_dir() {
    printf "%s" $(pwd | sed "s:$HOME:~:")
}

get_sha() {
    git rev-parse --short HEAD 2>/dev/null
}
# GIT PROMPT_COMMAND

# source ~/git-completion.bash

# GIT_PS1_SHOWDIRTYSTATE=1
# GIT_PS1_SHOWSTASHSTATE=1
# GIT_PS1_SHOWUNTRACKEDFILES=1
# # Explicitly unset color (default anyhow). Use 1 to set it.
# GIT_PS1_SHOWCOLORHINTS=1
# GIT_PS1_DESCRIBE_STYLE="branch"
# GIT_PS1_SHOWUPSTREAM="auto git"


# ------------------
# ------ PROMPT COMMAND
# ------------------
# PROMPT_COMMAND='__git_ps1 "\u \W" "\\\$ " " [%s $(get_sha)] "'

# PS1 Prompt
# # non-printable characters must be enclosed inside \[ and \]
# PS1='\[\033]0;$MSYSTEM:${PWD//[^[:ascii:]]/?}\007\]' # set window title
# PS1="$PS1"'\n'                 # new line
# PS1="$PS1""\[\e[0;36m\]"            # change color
# PS1="$PS1"'dir: \w'                 # current working directory

# PS1="$PS1"'\n'                 # new line

# # \e[38;5;ColorNumberm
# INFINITY="VI: ∞"   # my fancy unicode prompt
# PS1="$PS1""\[\e[0;33m\]"        # change color
# PS1="$PS1""$INFINITY "            # prompt: always {lambda}

# if test -z "$WINELOADERNOEXEC"
# then
#     PS1="$PS1"'$(__git_ps1) '   # bash function
# fi
# PS1="$PS1""\[\e[0m\]"            # change color
