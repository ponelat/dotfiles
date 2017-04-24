#!/bin/bash

i3-msg "workspace 1; append_layout ~/.config/i3/workspace_1.json"
i3-msg "workspace 2; append_layout ~/.config/i3/workspace_2.json"
i3-msg "workspace 3; append_layout ~/.config/i3/workspace_3.json"

# And finally we fill the containers with the programs they had
(emacsclient -c -a '' &)
(i3-sensible-terminal &)
(chrome &)

# Just a bonus
(startvpn &)
