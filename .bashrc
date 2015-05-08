CLONED_DIR="$HOME/manual"
PROJECTS="$HOME/projects"

# For passwords and such...
[[ -s $HOME/.env ]] && . $HOME/.env

# Add Python installed bins to path
export PATH+=:"$HOME/.local/bin"

# Our own bins
export PATH+=:"$HOME/dotfiles/bin"

# For switching accounts...
export PATH+=:"$HOME/projects/change_adsl"

# Java
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64/
export PATH+=:"$JAVA_HOME/bin"

# NVM ----------------------------------------------------
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# Sauce Credentials -------------------------------------
export SAUCE_USERNAME="ponelat"
export SAUCE_ACCESS_KEY="KEYKEYKEY"

# Source the 'rvm' function
. "$HOME/.rvm/scripts/rvm"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# Linuxbrew, see:
export PATH="$HOME/.linuxbrew/bin:$PATH"
export MANPATH="$(brew --prefix)/share/man:$MANPATH"
export INFOPATH="$(brew --prefix)/share/info:$INFOPATH"
export EDITOR=nvim

# Add this path for karma (I think)
export PHANTOMJS_BIN=`type -p phantomjs`

# Linux brew bash completions
for com in `\ls $(brew --prefix)/etc/profile.d/*`; do
  . $com
done

# Linuxbrew sourced files...
for com in `\ls $(brew --prefix)/etc/bash_completion.d/*`; do
  . $com
done

# Linuxbrew manually sourced files...
. "$(brew --prefix)/share/liquidprompt"

# Our own completions
if [[ -d $HOME/dotfiles/bash_completion.d  ]]; then
  for compl in `\ls $HOME/dotfiles/bash_completion.d/*` ; do
    . $compl
  done
fi

# Setup color support
# export TERM=xterm-256color
case "$TERM" in
       xterm*) TERM=xterm-256color
esac

# Base16 Shell
. "$CLONED_DIR/base16-shell/base16-default.dark.sh"

eval `dircolors`

# git  helpers aliases and functions
. $HOME/.githelpers

# git flow autocompletion
# source "$HOME/git-flow-completion.bash"

# Powerline
# powerline-daemon -q # this should be hiding in $HOME/.local/bin
# POWERLINE_BASH_CONTINUATION=1
# POWERLINE_BASH_SELECT=1
# source "$HOME/.local/lib/python2.7/site-packages/powerline/bindings/shell/powerline.sh"

## Aliases
alias ls='ls -A --color=always --group-directories-first -1 -v'
alias o='xdg-open'
alias n='nvim'
alias .b=". $HOME/dotfiles/.bashrc"
alias .color="(cd "$CLONED_DIR/base16-shell/" && . ./colortest)"
alias vib="vi $HOME/dotfiles/.bashrc"
alias nib="nvim $HOME/dotfiles/.bashrc"
alias ns="nvim -S"
alias dotinstall=". $HOME/dotfiles/install.sh"
alias specs="ln -s $PROJECTS/swagger-notes/specs dist/s"
alias impose="ln -fi ../swagger-notes/ui-gulpfile.js gulpfile.js"
alias ne='PATH=$(npm bin):$PATH'
alias damn_gitatt="echo '**/* binary' > .gitattributes && git add .gitattributes && git reset .gitattributes && git checkout .gitattributes"

## Local variables
# global -maxdepth for 'find'
FIND_MAX_DEPTH=5

# The following function has been moved into it's own file...
# Reboot into windows...
# function windows() {
#   # For this to work without requiring a password, run this...
#   # sudo visudo
#   # then add this to the bottom of sudoers
#   # josh    ALL=(ALL) NOPASSWD: /sbin/shutdown, /sbin/reboot, /sbin/poweroff, /usr/sbin/grub-reboot
#   sudo grub-reboot 4 && sudo reboot
# }


function entrs() {
  DIR=$(find . -type d | s)
  ls "$DIR"/* | entr "$*"
}

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
  cd $(find $HOME/projects -maxdepth 2 -type d | s)
}

function lss() {
  args=${1:-.}
  ls `find $args -maxdepth $FIND_MAX_DEPTH | s `
}

function movies() {
  args="/mnt/Data/Movies $HOME/Downloads"
  # xdg-open `find $args -maxdepth $FIND_MAX_DEPTH | s `

  TYPES_STR=avi
  TYPES="m4v mkv mp4"
  for T in $TYPES; do
    TYPES_STR=$TYPES_STR$T'\|'
  done

  xdg-open "`find $args -maxdepth $FIND_MAX_DEPTH -iregex '.*\.\('$TYPES_STR'\)' | s `"
}

function os() {
  args=${1:-.}
  xdg-open `find "$args" -maxdepth $FIND_MAX_DEPTH | s `
}

function cds() {
  # here we have a default value for our arg, go bash!
  # note: the hyphen is part of the syntax ....${var:-default}
  base=${1:-.}
  echo Limited to $FIND_MAX_DEPTH
  cd $(find_dir_ignore_common "$base" -maxdepth $FIND_MAX_DEPTH -type d | s)
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

function cpsa() {
  cps --all-folders "$*"
}

function cps() {
  limit=$FIND_MAX_DEPTH
  find_dir_command='find_dir_ignore_common'
  if [[ "$1" = "--all-folders" ]]; then
    find_dir_command='find_dir'
    shift
  fi
  src_base=${1:-.}
  dest_base=${2:-.}



  src_base_final="$($find_dir_command "$src_base" | s)"
  src=$(find $src_base_final -maxdepth $limit | s)
  dest=$($find_dir_command "$dest_base" | s)

  cp "$src" "$dest"
}

function cpss() {
  src_base=${1:-$HOME/projects}
  dest_base=${2:-.}
  cps "$src_base" "$dest_base"
}

function find_dir_ignore_common() {
  base=${1:-.}
  find $1 -type d \( ! -path "*node_modules*" ! -path '*bower_components*' ! -path '*git*' ! -path '*cache*' \)
}

function find_dir() {
  base=${1:-.}
  find "$1" -type d
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

# Gulp task autocompletion, needs to be after nvm setup
eval "$(gulp --completion=bash)"

# Grunt task autocompletion, needs to be after nvm setup
eval "$(grunt --completion=bash)"

###############################################################################
############ Github gits for ssh-agent
###############################################################################
# See: https://help.github.com/articles/working-with-ssh-key-passphrases#auto-launching-ssh-agent-on-msysgit
###############################################################################
# Note: $HOME/.ssh/environment should not be used, as it
#       already has a different purpose in SSH.

env=$HOME/.ssh/agent.env

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

# if your keys are not stored in $HOME/.ssh/id_rsa.pub or $HOME/.ssh/id_dsa.pub, you'll need
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


