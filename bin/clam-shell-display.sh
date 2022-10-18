#!/usr/bin/env bash
DISPLAY_NAME=$1

if grep -q open /proc/acpi/button/lid/LID/state; then
    swaymsg output $DISPLAY_NAME enable
else
    swaymsg output $DISPLAY_NAME disable
fi
