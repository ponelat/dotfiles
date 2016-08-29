dotfiles
========

## Notes...
#### /etc/sudoers
`josh	ALL=(ALL) NOPASSWD: /sbin/poweroff, /sbin/reboot, /sbin/shutdown`

#### Base16, 
with the base16-shell ( and a 256 term ) you need to source it, then run the function (once) to set the theme. Ie: Just read the readme... 
https://github.com/chriskempson/base16-shell
> Ie: you do not need https://github.com/chriskempson/base16-gnome-terminal !!!

#### Gnome Keyboard Shortcut
To launch a terminal emulator, with a startup script.
Create a custom keyboard shortcut, like so...
`x-terminal-emulator -e "/home/josh/projects/dotfiles/bin/attach_or_spawn_tmux.sh"`
