#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOTS="$DIR/dots"
homefiles=`find $DOTS` 

echo  Link all homefiles

# Each file on a separate line.
for _dotfile in $homefiles
do
  echo  ...Linking $_dotfile
  ln -i "$_dotfile" "$HOME/`basename $_dotfile`" # interactive, hard links
done

# Link .nvimrc as .vimrc
ln -i "$DOTS/.vimrc" "$HOME/.nvimrc" # interactive
# Link .nvim as .vim (in home)
#... create the dir if it doesn't exist
if [[ ! -d $HOME/.vim ]]; then
  echo 'making .vim'
  mkdir $HOME/.vim
fi
[ ! -d "$HOME/.nvim" ] && ln -s -i "$HOME/.vim" "$HOME/.nvim" # interactive

# Create link for bin
[ ! -d "$HOME/bin" ] && ln -s "$DIR/bin" "$HOME/bin"  # interactive

# echo Install gnome-terminal theme
# see: https://github.com/chriskempson/base16-builder
