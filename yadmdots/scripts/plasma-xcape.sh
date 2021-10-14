#!/bin/bash
# note - X11/keysymdef.h

#exit

echo $UDD xxxxxx
if [[ "$UDD" != "UDD" ]]; then
    /usr/bin/emacs -nw --with-profile terml --daemon=term&!
    sudo mount -a
fi

if [[ "$XDG_CURRENT_DESKTOP" == "GNOME" ]]; then
    notify-send "Not Plasma!"
    exit
fi

#stty -ixon

profile-cleaner f
profile-cleaner v

notify-send Plasma-xcape "Script starting"

#setxkbmap -layout gb -option ctrl:swapcaps # Caps Lock is Control on a GB keyboard

notify-send Plasma-xcape "Killing xcape instances prior to relaunch"
killall xcape &>/dev/null

function mykeyboard () {
	setxkbmap -option "terminate:ctrl_alt_bksp"
	#setxkbmap -option caps:none
	#setxkbmap -option "shift:both_capslock"
	#setxkbmap -option ctrl:nocaps
	xmodmap ~/.Xmodmap &
       	sleep 5
	xcape -e "Hyper_L=space" &
	sxhkd -c ~/.config/sxhkd/plasma-sxkhdrc&
	#emacs
	xcape -e "Shift_L=Control_L|space"&
}	

### Clear stale locks.
rm -f ~/emacs.d/.emacs.desktop.lock

#setxkbmap -option "caps:hyper"
#xcape -e 'Shift_L=Escape|BackSpace'&
#xcape -e "Shift_L=Control_L|1"&
#xcape -e 'Alt_L=Super_L|w'
#xcape -e "Control_L=Super_L|t"&

if [[ "`pgrep -c hydroxide`" == "0" ]]; then
    hydroxide imap&!
fi

sleep 2

#ntfy send "`qk status`"

#espanso start&!

fusuma&

FILE=~/.flags/no-kb-mods
[[ -f $FILE ]] || mykeyboard

#xmodmap -e "keycode  13 = 48 dollar 48 dollar"

notify-send "Running xcape processes: " `pgrep -c xcape`
notify-send "The end!"

exit 0
