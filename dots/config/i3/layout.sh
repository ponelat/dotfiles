#!/usr/bin/env bash

i3-msg "workspace 1; append_layout ~/.config/i3/workspace_term.json"
i3-msg "workspace 2; append_layout ~/.config/i3/workspace_emacs.json"
i3-msg "workspace 3; append_layout ~/.config/i3/workspace_chrome.json"

# And finally we fill the containers with the programs they had
(emacsclient -c -a '' &)
(i3-sensible-terminal &)
(chrome &)
