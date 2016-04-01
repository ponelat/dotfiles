#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOTS="$DIR/dots"
homefiles=`find $DOTS` 
CONFIG=${XDG_CONFIG_HOME:=$HOME/.config}
NVIM_HOME="$CONFIG/nvim"

echo  Link all homefiles

# Each file on a separate line.
for _dotfile in $homefiles
do
  echo  ...Linking $_dotfile
  ln -i "$_dotfile" "$HOME/`basename $_dotfile`" # interactive, hard links
done


if [[ ! -d $HOME/.vim ]]; then
  echo 'making .vim'
  mkdir $HOME/.vim
fi

# Setup nvim as links to vim
[ ! -d "$NVIM_HOME" ] && ln -s "$HOME/.vim" "$NVIM_HOME"
ln "$DOTS/.vimrc" "$NVIM_HOME/init.vim" 

# Create link for bin
[ ! -d "$HOME/bin" ] && ln -s "$DIR/bin" "$HOME/bin"  # interactive

# echo Install gnome-terminal theme
# see: https://github.com/chriskempson/base16-builder
