# Setup colors
source .256-color-terminal.sh

function subl(){ "/c/Program Files/Sublime Text 2/sublime_text" "$@" & }
alias ls='ls -A --color=always'
alias gl='git log --oneline --decorate '"$@"
alias gs='git status'"$@"
alias gb='git branch -v'"$@"
alias gc='git checkout '"$@"
alias gcb='git checkout -b'"$@"
alias gbm='git branch -v --merge'"$@"
# alias subl='"/c/Program Files/Sublime Text 2/sublime_text" &'

#export NODE_PATH="/c/Program Files/nodejs/node_modules"

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

# NVM ----------------------------------------------------
export NVM_DIR="/home/josh/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
# Sauce Credentials -------------------------------------       
  export SAUCE_USERNAME="ponelat"
  export SAUCE_ACCESS_KEY="2b212c69-8a63-4846-b0af-32a7fc68950e"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# added by travis gem
[ -f /home/josh/.travis/travis.sh ] && source /home/josh/.travis/travis.sh


# Set vi mode by default, probably should be the last line
# ...I've made it the second last 'section' so that I can add a VI mode in the prompt
set -o vi
set show-mode-in-prompt on

# non-printable characters must be enclosed inside \[ and \]
PS1='\[\033]0;$MSYSTEM:${PWD//[^[:ascii:]]/?}\007\]' # set window title
PS1="$PS1"'\n'                 # new line
PS1="$PS1""\[\e[0;36m\]"            # change color
PS1="$PS1"'dir: \w'                 # current working directory

PS1="$PS1"'\n'                 # new line

# \e[38;5;ColorNumberm
INFINITY="VI: ∞"   # my fancy unicode prompt
PS1="$PS1""\[\e[0;33m\]"        # change color
PS1="$PS1""$INFINITY "            # prompt: always {lambda}

if test -z "$WINELOADERNOEXEC"
then
    PS1="$PS1"'$(__git_ps1) '   # bash function
fi
PS1="$PS1""\[\e[0m\]"            # change color
