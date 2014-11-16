#!/usr/bin/env bash

homefiles=".bashrc .vimrc .tmux.conf"
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo  Link all homefiles

for _dotfile in $homefiles
do
  echo  ...Linking $_dotfile  # Each planet on a separate line.
  ln -i $DIR/$_dotfile ~/$_dotfile # interactive, hard links
done

# doneln .bashrc ~/.bashrc
# ln .tmux.conf ~/.tmux.conf
# ln .vimrc ~/.vimrc
