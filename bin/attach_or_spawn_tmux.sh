#!/usr/bin/env bash

# Try to attach to tmux, failing that spawn new instance
tmux -2 attach || tmux -2
