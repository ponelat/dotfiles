#!/usr/bin/env bash

homefiles=".agignore .ctags .gitconfig .githelpers .bashrc .bash_completion .vimrc .tmux.conf .xsessionrc .Xmodmap"
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo  Link all homefiles

# Each file on a separate line.
for _dotfile in $homefiles
do
  echo  ...Linking $_dotfile
  ln -i "$DIR/$_dotfile" "$HOME/$_dotfile" # interactive, hard links
done

# Link .nvimrc as .vimrc
ln -i "$DIR/.vimrc" "$HOME/.nvimrc" # interactive, hard links
# Link .nvim as .vim (in home)
#... create the dir if it dones't exist
if [[ ! -d $HOME/.vim ]]; then
  echo 'making .vim'
  mkdir $HOME/.vim
fi
ln -s -i "$HOME/.vim" "$HOME/.nvim" # interactive, hard links

# echo Install gnome-terminal theme
# see: https://github.com/chriskempson/base16-builder
