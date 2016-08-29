#!/usr/bin/env bash
echo Installing dotfiles... will quit on first error
set -e
thisDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Link the dotfiles
rcup -d $thisDir/dots -v

# Setup nvim as links to vim
CONFIG=${XDG_CONFIG_HOME:=$HOME/.config}
NVIM_HOME="$CONFIG/nvim"
[ ! -d "$NVIM_HOME" ] && ln -s "$HOME/.vim" "$NVIM_HOME"
[ ! -f "$NVIM_HOME/init.vim" ] && ln "$HOME/.vimrc" "$NVIM_HOME/init.vim" 

