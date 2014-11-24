#!/usr/bin/env bash

homefiles=".bashrc .vimrc .tmux.conf .Xresources"
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo  Link all homefiles

# Each file on a separate line.
for _dotfile in $homefiles
do
  echo  ...Linking $_dotfile  
  ln -i "$DIR/$_dotfile" "$HOME/$_dotfile" # interactive, hard links
done

# echo Install gnome-terminal theme
# see: https://github.com/chriskempson/base16-builder
