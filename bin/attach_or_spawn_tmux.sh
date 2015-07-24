#!/usr/bin/env bash

TMUX="/home/josh/.linuxbrew/bin/tmux"

# Try to attach to tmux, failing that spawn new instance
$TMUX -2 attach || $TMUX -2
