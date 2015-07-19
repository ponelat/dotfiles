#!/bin/sh

# Linux Mint hack to get a startup file to run...
DIR="$HOME/.config/autostart"
[ ! -d $DIR ] && echo $DIR does not exist... should I be doing this? && exit 1

ln -s "$HOME/dotfiles/xsession-hack.desktop" $DIR
chmod +x $DIR/xsession-hack.desktop
