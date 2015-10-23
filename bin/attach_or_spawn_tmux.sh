#!/usr/bin/env bash

TMUX="/home/josh/.linuxbrew/bin/tmux"

# Try to attach to tmux, failing that spawn new instance
# $TMUX -2 attach || $TMUX -2

# Create a new session, attach to '0' if it exists
$TMUX -2 new-session -t '0' || $TMUX -2 new-session -s '0'
