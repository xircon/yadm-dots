#!/bin/sh
bspc desktop -f ^9&

emacsclient -c ~/.config/bspwm/bspwmrc ~/.config/bspwm/mybspwm.sh ~/.config/sxhkd/sxhkdrc&
