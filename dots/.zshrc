
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set my terminal if I'm not in tmux.
[[ "$TMUX" == "" ]] && TERM=xterm-256color

ZSH_THEME="avit"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="false"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=~/dotfiles/zsh

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(brew npm fasd nvm docker)
# ssh-agent

# User configuration

PATH="/home/josh/bin:/home/josh/.linuxbrew/bin:.1.3/bin:/home/josh/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/josh/dotfiles/bin:/usr/lib/jvm/java-7-openjdk-amd64/bin"

PATH="$PATH:/home/josh/downloadedapps/robomongo-0.9.0-rc8-linux-x86_64-c113244/bin"
PATH="$PATH:/home/josh/downloadedapps/apache-maven-3.3.9/bin"

export PATH=$PATH
# export EDITOR="emacsclient -t"
# export ALTERNATE_EDITOR=""
# export VISUAL="emacsclient -c -a emacs"

export EDITOR="nvim"
export ALTERNATE_EDITOR=""
export VISUAL="nvim"




source $ZSH/oh-my-zsh.sh

export RPROMPT=


# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions

bindkey -v

setopt autolist       # Display completion candidates immediately.
setopt cdablevars     # When an argument should be a dir but is not one,
                      # expand it as if it started with ~ (see below).
setopt autonamedirs   # Any parameter that is set to an abosolute directory
                      # name automatically becomes a name for that directory
                      # in the form ~param.
setopt histignoredups # Do not record a command in the history if it is a
                      # duplicate of the previous one.
setopt listtypes      # When listing files that are possible completions,
                      # indicate their types with a trailing character.
setopt nolistbeep     # No bell on ambiguous completion!!


# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"
#
### User credentials (dont check this into your repo)
. ~/.env

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias n=nvim
alias ls='ls -A --color=always --group-directories-first -1 -v'
alias cs="clear; git status || ls "
alias .z=". ~/.zshrc"
alias j="fasd_cd -d"
alias .x="~/.xsession"
alias ne="PATH=$PATH:./node_modules/.bin; "
alias gg="git --no-pager l -30"
alias dcloud="docker run -it -v /usr/bin/docker:/usr/bin/docker -v /var/run/docker.sock:/var/run/docker.sock -e DOCKERCLOUD_USER=$DOCKERCLOUD_USER -e DOCKERCLOUD_APIKEY=$DOCKERCLOUD_APIKEY --rm dockercloud/cli"
alias be="bundle exec"
alias poston="pg_ctl start -D ~/.linuxbrew/var/postgres/ -l ~/.linuxbrew/var/postgres/log"
alias postoff="pg_ctl stop -D ~/.linuxbrew/var/postgres/"
alias postlog="cat ~/.linuxbrew/var/postgres/log"
alias encodeURIComponent='perl -pe '\''s/([^a-zA-Z0-9_.!~*()'\''\'\'''\''-])/sprintf("%%%02X",ord($1))/ge'\'
alias open="xdg-open"
alias fonts="fc-list | cut -f2 -d: | selecta"

export RBENV_ROOT=/home/josh/.linuxbrew/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# Base16 Shell
BASE16_SHELL="/home/josh/.config/base16-shell/base16-paraiso.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

fpath=(/home/josh/.linuxbrew/share/zsh-completions /home/josh/.oh-my-zsh/functions /home/josh/.oh-my-zsh/completions /usr/local/share/zsh/site-functions /home/josh/.linuxbrew/share/zsh/site-functions /home/josh/.linuxbrew/Cellar/zsh/5.1.1/share/zsh/functions)
