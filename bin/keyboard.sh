#!/bin/sh

# reset options
setxkbmap -rules "evdev" -model "pc105" -layout "us" -variant "dvorak" -option ""

# set up control and alt/win swapping
setxkbmap -rules "evdev" -model "pc105" -layout "us" -variant "dvorak" -option "caps:ctrl_modifier,altwin:swap_alt_win"

# fix the stupid tilde key for Mac keyboards
xmodmap -e "keycode  94 = grave asciitilde grave asciitilde grave asciitilde grave"
