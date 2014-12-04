# Add Python installed bins to path
export PATH+=:"$HOME/.local/bin"

# Setup color support
# export TERM=xterm-256color
case "$TERM" in
       xterm*) TERM=xterm-256color
esac

# Base16 Shell
BASE16_SHELL="/home/josh/.config/base16-shell/base16-default.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

eval `dircolors ~/.dircolors`

# git flow autocompletion
source "$HOME/git-flow-completion.bash"

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

alias ls='ls -A --color=always'
alias gl='git log --oneline --decorate '"$@"
alias glu='git log --oneline --decorate HEAD...'"$@"
alias gds='git diff --stat '"$@"
alias gd='git diff '"$@"
alias gb='git branch -v'"$@"
alias gc='git checkout '"$@"
alias gcb='git checkout -b'"$@"
alias gbm='git branch -v --merge'"$@"


export DEV=true
alias npm-exec='PATH=$(npm bin):$PATH'


# Run git status if in git repo, else ls -la
# see: https://gist.github.com/andrewberls/6119868#file-cs-sh
function cs {
  clear
  if ! git ls-files >& /dev/null
  then
    ls -la
  else
    git status
  fi
}

# NVM ----------------------------------------------------
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
# Sauce Credentials -------------------------------------       
  export SAUCE_USERNAME="ponelat"
  export SAUCE_ACCESS_KEY="2b212c69-8a63-4846-b0af-32a7fc68950e"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# Gulp task autocompletion
eval "$(gulp --completion=bash)"


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
