# Path to your oh-my-zsh installation.
  export ZSH=/home/josh/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="amuse"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="false"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

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
plugins=(brew npm web-search fasd nvm docker)
# ssh-agent

# User configuration

  export PATH="/home/josh/.nvm/versions/node/v0.12.7/bin:/home/josh/bin:/home/josh/.linuxbrew/bin:/usr/local/rvm/gems/ruby-2.1.3/bin:/usr/local/rvm/gems/ruby-2.1.3@global/bin:/usr/local/rvm/rubies/ruby-2.1.3/bin:/home/josh/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/usr/local/rvm/bin:/home/josh/dotfiles/bin:/usr/lib/jvm/java-7-openjdk-amd64//bin:/home/josh/.rvm/bin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='nvim'
bindkey -v

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
alias dtutum="docker run -it -v /usr/bin/docker:/usr/bin/docker -v /var/run/docker.sock:/var/run/docker.sock -e TUTUM_USER=$TUTUM_USER -e TUTUM_APIKEY=$TUTUM_APIKEY --rm tutum/cli"
alias dcloud="docker run -it -v /usr/bin/docker:/usr/bin/docker -v /var/run/docker.sock:/var/run/docker.sock -e DOCKERCLOUD_USER=$DOCKERCLOUD_USER -e DOCKERCLOUD_PASS=$DOCKERCLOUD_PASS --rm dockercloud/cli"

export RBENV_ROOT=/home/josh/.linuxbrew/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/base16-default.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

if [ -f "$(brew --prefix scm_breeze)/scm_breeze.sh" ]; then
  source "$(brew --prefix scm_breeze)/scm_breeze.sh"
fi

fpath=(/home/josh/.linuxbrew/share/zsh-completions /home/josh/.oh-my-zsh/functions /home/josh/.oh-my-zsh/completions /usr/local/share/zsh/site-functions /home/josh/.linuxbrew/share/zsh/site-functions /home/josh/.linuxbrew/Cellar/zsh/5.1.1/share/zsh/functions)
